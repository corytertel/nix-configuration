{
  description = "Cory's system configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-unstable.url = "nixpkgs/master";
    systems.url = "github:nix-systems/default";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.systems.follows = "systems";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    nixos-wsl.url = "github:nix-community/NixOS-WSL";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
    nixos-wsl.inputs.flake-utils.follows = "flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { nixpkgs, home-manager, nur, nixos-wsl, emacs-overlay, ... }:
    let
      system = "x86_64-linux";

      config = nixpkgs.config;
      lib = nixpkgs.lib;

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [ nur.overlay emacs-overlay.overlay ]
                   ++ import ./overlays { inherit pkgs; }
                   ++ import ./packages { inherit config lib pkgs; };
      } // { outPath = nixpkgs.outPath; };

      mkHost =
        { machineConfig
        , isContainer ? false
        }:
        lib.nixosSystem {
          inherit system pkgs;
          specialArgs = {
            inherit isContainer;
          };
          modules = machineConfig ++ [
            ./modules
            home-manager.nixosModules.home-manager {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
            }
          ];
        };

    in {
      devShell.${system} = import ./shell.nix { inherit pkgs; };

      nixosConfigurations = {
        pc = mkHost {
          machineConfig = [ ./hosts/pc  ];
        };

        laptop = mkHost {
          machineConfig = [ ./hosts/laptop ];
        };

        vm = mkHost {
          machineConfig = [ ./hosts/vm ];
        };

        wsl = mkHost {
          isContainer = true;
          machineConfig = [
            ./hosts/wsl
            nixos-wsl.nixosModules.wsl
          ];
        };
      };
    };
}
