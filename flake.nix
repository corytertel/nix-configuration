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
      overlays = [
        (import ./overlays/blacknord-gtk.nix { inherit config pkgs lib; })
        (import ./overlays/parchment-gtk.nix { inherit config pkgs lib; })
        (import ./overlays/mountain-gtk.nix { inherit config pkgs lib; })
        (import ./overlays/plainlight-gtk.nix { inherit config pkgs lib; })
      ];
    };

    config = nixpkgs.config;
    lib = nixpkgs.lib;

  in {
    homeManagerConfigurations = {
      pc = home-manager.lib.homeManagerConfiguration {
        inherit system pkgs;
        username = "cory";
        stateVersion = "22.05";
        homeDirectory = "/home/cory";
        configuration = {
          imports = [
            ./pc/home.nix
            ./shared/home.nix
            ./rices/light/home.nix
          ];
        };
      };

      laptop = home-manager.lib.homeManagerConfiguration {
        inherit system pkgs;
        username = "cory";
        stateVersion = "22.05";
        homeDirectory = "/home/cory";
        configuration = {
          imports = [
            ./laptop/home.nix
            ./shared/home.nix
            ./rices/functional/home.nix
          ];
        };
      };
    };

    nixosConfigurations = {
      pc = lib.nixosSystem {
        inherit system;
        modules = [
          ./pc/configuration.nix
          ./shared/configuration.nix
          ./rices/light/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };

      laptop = lib.nixosSystem {
        inherit system;
        modules = [
          ./laptop/configuration.nix
          ./shared/configuration.nix
          ./rices/functional/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };
    };
  };
}
