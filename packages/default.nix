{ config, lib, pkgs, ... }:

[
  (import ./blacknord-gtk.nix { inherit config pkgs lib; })
  (import ./keyboard-layouts.nix { inherit pkgs; })
  (import ./mountain-gtk.nix { inherit config pkgs lib; })
  (import ./new-tab-override.nix { inherit lib pkgs; })
  (import ./nf-noto.nix { inherit lib pkgs; })
  (import ./nf-victormono.nix { inherit lib pkgs; })
  (import ./parchment-gtk.nix { inherit config pkgs lib; })
  (import ./plainlight-gtk.nix { inherit config pkgs lib; })
  (import ./photogimp.nix { inherit lib pkgs; })
  (import ./sddm-mountain-light.nix { inherit pkgs; })
]
