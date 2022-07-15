{ config, lib, pkgs, ... }:

[
  (import ./crystal-nova.nix { inherit lib pkgs; })
  (import ./crystal-remix.nix { inherit lib pkgs; })
  (import ./expose-glassy.nix)
  (import ./expose-glassy-right.nix)
  (import ./firefox-classic-theme.nix { inherit lib pkgs; })
  (import ./keyboard-layouts.nix { inherit pkgs; })
  (import ./krunner-desktop.nix { inherit pkgs; })
  (import ./layout-switch.nix {inherit pkgs; })
  (import ./newaita-reborn.nix { inherit lib pkgs; })
  (import ./new-tab-override.nix { inherit lib pkgs; })
  (import ./nf-noto.nix { inherit lib pkgs; })
  (import ./nf-oxygen.nix { inherit lib pkgs; })
  (import ./nf-victormono.nix { inherit lib pkgs; })
  (import ./nova7.nix { inherit lib pkgs; })
  (import ./oxygen-kde4-theme.nix)
  (import ./photogimp.nix { inherit lib pkgs; })
  (import ./sddm-mountain-light.nix { inherit pkgs; })
  (import ./trim-generations { inherit pkgs; })
  (import ./undistract-me-zsh.nix { inherit pkgs; })
]
