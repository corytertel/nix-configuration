{ config, lib, pkgs, ... }:

[
  (import ./blacknord-gtk.nix { inherit config pkgs lib; })
  (import ./crystal-remix.nix { inherit lib pkgs; })
  (import ./keyboard-layouts.nix { inherit pkgs; })
  (import ./mountain-gtk.nix { inherit config pkgs lib; })
  (import ./newaita-reborn.nix { inherit lib pkgs; })
  (import ./new-tab-override.nix { inherit lib pkgs; })
  (import ./nf-noto.nix { inherit lib pkgs; })
  (import ./nf-oxygen.nix { inherit lib pkgs; })
  (import ./nf-victormono.nix { inherit lib pkgs; })
  (import ./nomanssky-theme.nix)
  (import ./oxygen-gtk.nix)
  (import ./oxygen-kde4-theme.nix)
  (import ./parchment-gtk.nix { inherit config pkgs lib; })
  (import ./plainlight-gtk.nix { inherit config pkgs lib; })
  (import ./photogimp.nix { inherit lib pkgs; })
  (import ./sddm-mountain-light.nix { inherit pkgs; })
]
