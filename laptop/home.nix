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
