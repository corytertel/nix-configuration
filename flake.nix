{
  description = "Cory's system configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
  };

  outputs = { nixpkgs, home-manager, nix-doom-emacs, ... }: 
  let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
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
            ./shared/users/cory/home.nix
            ./pc/users/cory/home.nix
            nix-doom-emacs.hmModule
          ];
          programs.doom-emacs = {
            enable = false;
            doomPrivateDir = ./shared/users/cory/apps/emacs/doom.d;
          };
          programs.emacs.enable = true;
        };
      };

      laptop = home-manager.lib.homeManagerConfiguration {
        inherit system pkgs;
        username = "cory";
        stateVersion = "21.05";
        homeDirectory = "/home/cory";
        configuration = {
          imports = [
            ./shared/users/cory/home.nix
            ./laptop/users/cory/home.nix
            nix-doom-emacs.hmModule
          ];
          programs.doom-emacs = {
            enable = false;
            doomPrivateDir = ./shared/users/cory/apps/emacs/doom.d;
          };
          programs.emacs.enable = true;
        };
      };
    };

    nixosConfigurations = {
      # use the host name
      pc = lib.nixosSystem {
        inherit system;

        modules = [
          ./pc/system/configuration.nix
          ./shared/system/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };

      laptop = lib.nixosSystem {
        inherit system;

        modules = [
          ./laptop/system/configuration.nix
          ./shared/system/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };
    };

  };
}
