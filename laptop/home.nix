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

    ! Config
    URxvt*termName: rxvt
    URxvt*visualBell: false
    URxvt*loginShell: true

    URxvt*colorMode: true
    URxvt*pointerColor: #f0f0f0
    URxvt*pointerColor2: #c49ea0

    ! Mountain Xresources palette
    URxvt*foreground: #f0f0f0
    URxvt*background: [98]#0f0f0f
    URxvt*cursorColor: #f0f0f0
    URxvt*color0:     #262626
    URxvt*color8:     #4c4c4c
    URxvt*color1:     #ac8a8c
    URxvt*color9:     #c49ea0
    URxvt*color2:     #8aac8b
    URxvt*color10:    #9ec49f
    URxvt*color3:     #aca98a
    URxvt*color11:    #c4c19e
    URxvt*color4:     #8f8aac
    URxvt*color12:    #a39ec4
    URxvt*color5:     #ac8aac
    URxvt*color13:    #c49ec4
    URxvt*color6:     #8aabac
    URxvt*color14:    #9ec3c4
    URxvt*color7:     #e7e7e7
    URxvt*color15:    #f5f5f5

    Xcursor.size: 11

    !! URxvt Appearance
    URxvt.font: xft:FantasqueSansMono Nerd Font:style=Regular:size=11
    URxvt.boldFont: xft:FantasqueSansMono Nerd Font:style=Bold:size=11
    URxvt.italicFont: xft:FantasqueSansMono Nerd Font:style=Italic:size=11
    URxvt.boldItalicFont: xft:FantasqueSansMono Nerd Font:style=Bold Italic:size=11
    URxvt.letterSpace: 0
    URxvt.lineSpace: 0
    URxvt.geometry: 92x24
    URxvt.internalBorder: 24
    URxvt.cursorBlink: true
    URxvt.cursorUnderline: false
    URxvt.saveline: 2048
    URxvt.scrollBar: false
    URxvt.scrollBar_right: false
    URxvt.urgentOnBell: true
    !URxvt.depth: 24
    URxvt.depth: 32
    URxvt.iso14755: false

    !! Common Keybinds for Navigations
    URxvt.keysym.Shift-Up: command:\033]720;1\007
    URxvt.keysym.Shift-Down: command:\033]721;1\007
    URxvt.keysym.Control-Up: \033[1;5A
    URxvt.keysym.Control-Down: \033[1;5B
    URxvt.keysym.Control-Right: \033[1;5C
    URxvt.keysym.Control-Left: \033[1;5D
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
