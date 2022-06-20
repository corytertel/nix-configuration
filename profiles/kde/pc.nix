{ config, lib, pkgs, ... }:

{
  imports = [ ./shared.nix ];

  theme.cursor = {
    theme = "Vanilla-DMZ-AA";
    size = 32;
    package = pkgs.vanilla-dmz;
  };

  theme.wallpaper = ./wallpapers/maui.jpg;
}
