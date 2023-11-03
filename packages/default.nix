{ config, lib, pkgs, ... }:

[
  # (import ./chicken-docs.nix { inherit pkgs; })
  (import ./apl385.nix { inherit lib pkgs; })
  (import ./chicken-pkgs.nix { inherit pkgs; })
  (import ./crystal-nova.nix { inherit lib pkgs; })
  (import ./crystal-remix.nix { inherit lib pkgs; })
  (import ./dyalog.nix { inherit lib pkgs; })
  (import ./expose-glassy.nix)
  (import ./expose-glassy-right.nix)
  (import ./firefox-classic-theme.nix { inherit lib pkgs; })
  (import ./firefox-oxygen-theme.nix { inherit lib pkgs; })
  (import ./iosevka-aile.nix {inherit pkgs; })
  (import ./iosevka-etoile.nix {inherit pkgs; })
  (import ./iosevka-slab.nix {inherit pkgs; })
  (import ./jdtls.nix { inherit pkgs; })
  (import ./keyboard-layouts { inherit pkgs; })
  (import ./layout-switch.nix {inherit pkgs; })
  (import ./newaita-reborn.nix { inherit lib pkgs; })
  (import ./new-tab-override.nix { inherit lib pkgs; })
  (import ./nf-julia.nix { inherit lib pkgs; })
  (import ./nf-noto.nix { inherit lib pkgs; })
  (import ./nf-oxygen.nix { inherit lib pkgs; })
  (import ./nf-victormono.nix { inherit lib pkgs; })
  (import ./nova7.nix { inherit lib pkgs; })
  (import ./oxygen-cory-colors)
  (import ./oxygen-kde4-theme.nix)
  (import ./photogimp.nix { inherit lib pkgs; })
  (import ./ride.nix { inherit lib pkgs; })
  (import ./sddm-oxygen.nix { inherit pkgs; })
  (import ./trim-generations { inherit pkgs; })
  (import ./undistract-me-zsh.nix { inherit pkgs; })
]
