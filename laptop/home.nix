{ config, pkgs, ... }:

{
  imports =
    [
    ];

  # Xresources
  # DPI settings
  # Set DPI to the scale you want your applications at
  # 175 for desktop, 250 for laptop typically
  xresources.extraConfig = ''
    Xft.dpi: 225
    Xft.antialias: 1
    Xft.hinting: 1
    Xft.autohint: 0
    Xft.hintstyle: hintslight
    Xft.rgba: rgb
    Xft.lcdfilter: lcddefault

    Sxiv.background: #ffffff
    Sxiv.foreground: #141404
    Sxiv.font:mplus Nerd Font,M+ 1c:size=10
  '';

  xsession = {
    enable = true;
    pointerCursor = {
      # name = "Numix-Cursor";
      # package = pkgs.numix-cursor-theme;
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      defaultCursor = "left_ptr";
      size = 48;
    };
  };

  home.packages = with pkgs; [
    zoom-us
  ];
}
