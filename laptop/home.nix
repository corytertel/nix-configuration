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
    Xft.dpi: 225
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

  xsession = {
    enable = true;
    pointerCursor = {
      # name = "Numix-Cursor";
      # package = pkgs.numix-cursor-theme;
      name = "Vanilla-DMZ-AA";
      package = pkgs.vanilla-dmz;
      defaultCursor = "left_ptr";
      size = 48;
    };
  };

  home.packages = with pkgs; [
    zoom-us
  ];
}
