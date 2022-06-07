{ config, pkgs, ... }:

{
  imports = [
    ./shared.nix
  ];

  xresources.extraConfig = ''
    Xft.dpi: 150
    Xft.antialias: 1
    Xft.hinting: 1
    Xft.autohint: 0
    Xft.hintstyle: hintslight
    Xft.rgba: rgb
    Xft.lcdfilter: lcddefault
  '';

  home.pointerCursor = {
    name = "Vanilla-DMZ-AA";
    size = 32;
    gtk.enable = true;
    package = pkgs.vanilla-dmz;
    x11 = {
      enable = true;
      defaultCursor = "left_ptr";
    };
  };

  home.packages = with pkgs; [
      airshipper
      steamPackages.steamcmd
      minecraft
  ];
}
