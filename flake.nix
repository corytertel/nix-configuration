{
  description = "Cory's system configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    neovim-git.url = "github:neovim/neovim?dir=contrib";
  };

  outputs = { nixpkgs, home-manager, neovim-git, ... }:
  let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
      config = { allowUnfree = true; };
      overlays = [
          neovim-git.overlay
          #(import ./overlays)
        ];
    };

    lib = nixpkgs.lib;

  in {
    homeManagerConfigurations = {
      pc = home-manager.lib.homeManagerConfiguration {
        inherit system pkgs;
        username = "cory";
        stateVersion = "21.11";
        homeDirectory = "/home/cory";
        configuration = {
          imports = [
            ./pc/home.nix
            ./shared/home.nix
            ./rices/retro/home.nix
          ];
        };
      };

      laptop = home-manager.lib.homeManagerConfiguration {
        inherit system pkgs;
        username = "cory";
        stateVersion = "21.11";
        homeDirectory = "/home/cory";
        configuration = {
          imports = [
            ./laptop/home.nix
            ./shared/home.nix
            ./rices/minimal/home.nix
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
          ./rices/retro/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
          # ({pkgs, config, ...}: {
          #   config = {
          #     nix = {
          #       # add binary caches
          #       binaryCachePublicKeys = [
          #         "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          #         "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
          #         # ...
          #       ];
          #       binaryCaches = [
          #         "https://cache.nixos.org"
          #         "https://nixpkgs-wayland.cachix.org"
          #         # ...
          #       ];
          #     };

          #     # use it as an overlay
          #     nixpkgs.overlays = [ nixpkgs-wayland.overlay ];

          #     # pull specific packages (built against inputs.nixpkgs, usually `nixos-unstable`)
          #     environment.systemPackages = with pkgs; [
          #       nixpkgs-wayland.packages.${system}.waybar
          #     ];
          #   };
          # })
        ];
      };

      laptop = lib.nixosSystem {
        inherit system;
        modules = [
          ./laptop/configuration.nix
          ./shared/configuration.nix
          ./rices/minimal/configuration.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
        ];
      };
    };

  };
}
