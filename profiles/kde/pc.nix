{ config, lib, pkgs, ... }:

{
  imports = [ ./shared.nix ];

  theme.cursor = {
    theme = "Oxygen_White";
    size = 48;
  };

  theme.wallpaper = ./wallpapers/gatekeeper.jpg;
}
