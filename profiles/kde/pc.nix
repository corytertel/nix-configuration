{ config, lib, pkgs, ... }:

{
  imports = [ ./shared.nix ];

  windowManagers.cory.kde.config = [
    (import ../../config/kde/shared.nix { inherit config; })
    (import ../../config/kde/pc.nix)
  ];

  programs.cory.discord.package = pkgs.discord-gpu;

  theme.cursor = {
    theme = "Oxygen_White";
    size = 48;
  };

  theme.wallpaper = ./wallpapers/honolulu.jpg;

  # File Manager
  programs.cory.dolphin.config = import ../../config/dolphin/pc.nix;

  # Launcher
  services.cory.rofi.config = let
    offsetX = -94;
  in "${import ../../config/rofi/launcher.nix { inherit config pkgs offsetX; }}";
}
