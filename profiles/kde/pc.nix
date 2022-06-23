{ config, lib, pkgs, ... }:

{
  imports = [ ./shared.nix ];

  programs.cory.discord.package = pkgs.discord-gpu;

  theme.cursor = {
    theme = "Oxygen_White";
    size = 48;
  };

  theme.wallpaper = ./wallpapers/gatekeeper.jpg;
}
