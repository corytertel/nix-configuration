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
}
