{ config, lib, pkgs, ... }:

{
  imports = [ ./shared.nix ];

  windowManagers.cory.kde.config = [
    (import ../../config/kde/shared.nix { inherit config; })
    (import ../../config/kde/laptop.nix)
  ];

  theme.cursor = {
    theme = "Oxygen_White";
    size = 72;
  };

  theme.wallpaper = ./wallpapers/maui2.jpg;

  # File Manager
  programs.cory.dolphin.config = import ../../config/dolphin/laptop.nix;

  # Gestures
  services.cory.touchegg = {
    enable = true;
    config = ../../config/touchegg/kde.conf;
  };

  # Launcher
  services.cory.rofi.config = let
    offsetX = -240;
  in "${import ../../config/rofi/launcher.nix { inherit config pkgs offsetX; }}";
}
