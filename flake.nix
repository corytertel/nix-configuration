{
  description = "Cory's system configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    home-manager.url = "github:nix-community/home-manager/master";
    # nixpkgs.url = "nixpkgs/nixos-21.11";
    # nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    # home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, home-manager, ... }:
  let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
      overlays = import ./overlays { inherit pkgs; }
                 ++ import ./packages { inherit config lib pkgs; };
    };

    config = nixpkgs.config;
    lib = nixpkgs.lib;

  in {
    devShell.${system} = import ./shell.nix { inherit pkgs; };

    # homeManagerConfigurations = {
    #   pc = home-manager.lib.homeManagerConfiguration {
    #     inherit system pkgs;
    #     username = "cory";
    #     stateVersion = "22.05";
    #     homeDirectory = "/home/cory";
    #     configuration = {
    #       imports = [
    #         ./pc/home.nix
    #         ./shared/home.nix
    #         ./wm/fvwm-pc/home.nix
    #       ];
    #     };
    #   };

    #   laptop = home-manager.lib.homeManagerConfiguration {
    #     inherit system pkgs;
    #     username = "cory";
    #     stateVersion = "22.05";
    #     homeDirectory = "/home/cory";
    #     configuration = {
    #       imports = [
    #         ./laptop/home.nix
    #         ./shared/home.nix
    #         ./wm/xmonad-laptop/home.nix
    #       ];
    #     };
    #   };
    # };

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
