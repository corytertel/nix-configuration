{
  description = "Cory's system configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { nixpkgs, home-manager, emacs-overlay, ... }: 
  let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
      overlays = [
          emacs-overlay.overlay
          #(import ./overlays)
        ];
    };

    lib = nixpkgs.lib;

  in {
    homeManagerConfigurations = {
      #entry for each user account
      pc = home-manager.lib.homeManagerConfiguration {
        inherit system pkgs;
        username = "cory";
        stateVersion = "21.05";
        homeDirectory = "/home/cory";
        configuration = {
          imports = [
            ./pc/users/cory/home.nix
            ./shared/users/cory/home.nix
            ./rices/mountain/home.nix
          ];
        };
      };

      laptop = home-manager.lib.homeManagerConfiguration {
        inherit system pkgs;
        username = "cory";
        stateVersion = "21.05";
        homeDirectory = "/home/cory";
        configuration = {
          imports = [
            ./laptop/users/cory/home.nix
            ./shared/users/cory/home.nix
            ./rices/tree/home.nix
          ];
        };
      };
    };

    nixosConfigurations = {
      # use the host name
      pc = lib.nixosSystem {
        inherit system;

        modules = [
          ./pc/configuration.nix
          ./shared/configuration.nix
          ./rices/mountain/configuration.nix
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
          ./rices/tree/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };
    };

  };
}
