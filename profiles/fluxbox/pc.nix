{ config, lib, pkgs, ... }:

{
  # PC Stuff
  programs.cory.discord.package = pkgs.discord-gpu;

  theme.cursor = {
    theme = "Oxygen_White";
    size = 48;
  };

  # File Manager
  programs.cory.dolphin.config = import ../../config/dolphin/pc.nix;

  # Launcher
  services.cory.rofi.config = let
    offsetX = -240;
  in "${import ../../config/rofi/launcher.nix { inherit config pkgs offsetX; }}";
}
