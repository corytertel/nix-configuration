{ config, pkgs, ... }:

{
  imports = [
    ./shared.nix
  ];

  xresources.extraConfig = ''
    Xft.dpi: 225
    Xft.antialias: 1
    Xft.hinting: 1
    Xft.autohint: 0
    Xft.hintstyle: hintslight
    Xft.rgba: rgb
    Xft.lcdfilter: lcddefault
  '';

  home.pointerCursor = {
    # name = "Numix-Cursor";
    # name = "Vanilla-DMZ-AA";
    name = "Oxygen_White";
    size = 48;
    gtk.enable = true;
    # package = pkgs.numix-cursor-theme;
    # package = pkgs.vanilla-dmz;
    package = pkgs.libsForQt5.oxygen;
    x11 = {
      enable = true;
      defaultCursor = "left_ptr";
    };
  };

  home.packages = with pkgs; [
    zoom-us
  ];
}
