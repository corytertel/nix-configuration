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
    Xft.dpi: 250
    ! Mountain Xresources palette
    *.foreground: #f0f0f0
    *.background: #0f0f0f
    *.color0:     #262626
    *.color8:     #4c4c4c
    *.color1:     #ac8a8c
    *.color9:     #c49ea0
    *.color2:     #8aac8b
    *.color10:    #9ec49f
    *.color3:     #aca98a
    *.color11:    #c4c19e
    *.color4:     #8f8aac
    *.color12:    #a39ec4
    *.color5:     #ac8aac
    *.color13:    #c49ec4
    *.color6:     #8aabac
    *.color14:    #9ec3c4
    *.color7:     #e7e7e7
    *.color15:    #f5f5f5
  '';

  # Fix pointer cursor
  xsession = {
    enable = true;
    pointerCursor = {
      #name = "Bibata_Amber";
      #package = pkgs.bibata-cursors;
      #defaultCursor = "left_ptr";
      #size = 36;
      name = "Adwaita";
      package = pkgs.gnome3.adwaita-icon-theme;
      defaultCursor = "left_ptr";
      size = 48;
    };
  };

  home.packages = with pkgs; [
    discord
    betterdiscord-installer
    betterdiscordctl
  ];
}