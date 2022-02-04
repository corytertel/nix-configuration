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
    Xft.dpi: 150
    Xft.antialias: 1
    Xft.hinting: 1
    Xft.autohint: 0
    Xft.hintstyle: hintslight
    Xft.rgba: rgb
    Xft.lcdfilter: lcddefault

    Sxiv.background: #000507
    Sxiv.foreground: #d8dee9
    Sxiv.font:M+ 1c:size=10
  '';

  # Fix pointer cursor
  xsession = {
    enable = true;
    pointerCursor = {
      #name = "Bibata_Amber";
      #package = pkgs.bibata-cursors;
      #defaultCursor = "left_ptr";
      #size = 36;
      #name = "Adwaita";
      #package = pkgs.gnome3.adwaita-icon-theme;
      #defaultCursor = "left_ptr";
      #size = 32;
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      defaultCursor = "left_ptr";
      size = 32;
    };
  };

  home.packages = with pkgs; [
  ];
}
