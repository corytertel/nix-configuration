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
    Sxiv.font:VictorMono Nerd Font:size=10
  '';

  # Fix pointer cursor
  xsession = {
    enable = true;
    pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      defaultCursor = "left_ptr";
      size = 32;
    };
  };

  home.packages = with pkgs; [
      airshipper
      steamPackages.steamcmd
      minecraft
  ];
}
