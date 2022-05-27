{
  description = "Cory's system configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    nur.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, home-manager, nur, ... }:
  let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
      overlays = [ nur.overlay ]
        ++ import ./overlays { inherit pkgs; }
        ++ import ./packages { inherit config lib pkgs; };
    };

    config = nixpkgs.config;
    lib = nixpkgs.lib;

  in {
    devShell.${system} = import ./shell.nix { inherit pkgs; };
    nixosConfigurations = {
      pc = lib.nixosSystem {
        inherit system pkgs;
        modules = [
          ./hosts/pc
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.cory = import ./modules/pc.nix;
          }
        ];
      };

      laptop = lib.nixosSystem {
        inherit system pkgs;
        modules = [
          ./hosts/laptop
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.cory = import ./modules/laptop.nix;
          }
        ];
      };
    };
  };
}
