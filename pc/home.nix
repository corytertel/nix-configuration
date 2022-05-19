{ config, pkgs, ... }:

{
  imports =
    [
    ];

  # Xresources
  # DPI settings
  # Set DPI to the scale you want your applications at
  # 150 for desktop, 225 for laptop typically
  xresources.extraConfig = ''
    Xft.dpi: 150
    Xft.antialias: 1
    Xft.hinting: 1
    Xft.autohint: 0
    Xft.hintstyle: hintslight
    Xft.rgba: rgb
    Xft.lcdfilter: lcddefault

    Sxiv.background: #ffffff
    Sxiv.foreground: #141404
    Sxiv.font:NotoSans Nerd Font:size=10
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
