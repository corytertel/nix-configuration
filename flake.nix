{
  description = "Cory's system configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.05";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, home-manager, ... }: 
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
            ./users/pc/home.nix
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
            ./users/laptop/home.nix
          ];
        };
      };
    };

    nixosConfigurations = {
      # use the host name
      pc = lib.nixosSystem {
        inherit system;

        modules = [
          ./system/pc/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            #home-manager.users.cory = import ./users/cory/home.nix;
          }
        ];
      };

      laptop = lib.nixosSystem {
        inherit system;

        modules = [
          ./system/laptop/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            #home-manager.users.cory = import ./users/cory/home.nix;
          }
        ];
      };
    };

  };
}
