{ config, lib, pkgs, ... }:

{
  # Cursor
  theme.cursor = {
    theme = "Vanilla-DMZ";
    size = 32;
    package = pkgs.vanilla-dmz;
  };

  # File Manager
  programs.cory.dolphin.config = import ../../config/dolphin/laptop.nix;

  # Gestures
  services.cory.touchegg = {
    enable = true;
    config = ../../config/touchegg/fvwm.conf;
  };
}
