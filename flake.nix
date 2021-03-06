{
  description = "Cory's system configuration";

  inputs = {
    # Unstable Branch
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    home-manager.url = "github:nix-community/home-manager/master";

    # Stable Branch
    # nixpkgs.url = "nixpkgs/nixos-22.05";
    # nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    # home-manager.url = "github:nix-community/home-manager/release-22.05";

    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { nixpkgs, home-manager, nur, emacs-overlay, ... }:
  let
    system = "x86_64-linux";

    config = nixpkgs.config;
    lib = nixpkgs.lib;

    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
      overlays = [ nur.overlay emacs-overlay.overlay ]
        ++ import ./overlays { inherit pkgs; }
        ++ import ./packages { inherit config lib pkgs; };
    };

  in {
    devShell.${system} = import ./shell.nix { inherit pkgs; };
    nixosConfigurations = {
      pc = lib.nixosSystem {
        inherit system pkgs;
        modules = [
          ./modules
          ./hosts/pc
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };

      laptop = lib.nixosSystem {
        inherit system pkgs;
        modules = [
          ./modules
          ./hosts/laptop
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };

      vm = lib.nixosSystem {
        inherit system pkgs;
        modules = [
          ./modules
          ./hosts/vm
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };
    };
  };
}
