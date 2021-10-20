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

    pc_rice = "mountain";
    laptop_rice = "tree";

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
            ./rices/${pc_rice}/home.nix
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
            ./rices/${laptop_rice}/home.nix
          ];
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
          ./rices/${pc_rice}/configuration.nix
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
          ./rices/${laptop_rice}/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };
    };

  };
}
