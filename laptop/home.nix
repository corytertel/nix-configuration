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

    Sxiv.background: #000507
    Sxiv.foreground: #d8dee9
    Sxiv.font:JetBrainsMono Nerd Font:size=10
  '';

  xsession = {
    enable = true;
    pointerCursor = {
      name = "Numix-Cursor";
      package = pkgs.numix-cursor-theme;
      defaultCursor = "left_ptr";
      size = 48;
    };
  };

  home.packages = with pkgs; [
    touchegg

    zoom-us
  ];
}
