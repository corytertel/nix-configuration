{ config, lib, pkgs, ... }:

{
  # PC Stuff
  programs.cory.discord.package = pkgs.discord-gpu;

  theme.cursor = {
    theme = "Oxygen_White";
    size = 72;
    package = pkgs.libsForQt5.oxygen;
  };

  # File Manager
  programs.cory.dolphin.config = import ../../config/dolphin/laptop.nix;

  # Gestures
  services.cory.touchegg = {
    enable = true;
    config = ../../config/touchegg/fvwm.conf;
  };

  # Launcher
  services.cory.rofi.config = let
    offsetX = -240;
  in "${import ../../config/rofi/launcher.nix { inherit config pkgs offsetX; }}";
}
