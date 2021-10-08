{ config, pkgs, ... }:

{
  imports =
    [
      ./apps/kitty
      ./apps/bash
      ./apps/dunst
      ./apps/zathura
      ./apps/rofi
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

  services.picom = {
    enable = true;
    inactiveOpacity = "1.00";
    activeOpacity = "1.00";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      "98:class_g   *?= 'emacs'"
      #"90:class_g   *?= 'discord'"
      #"75:class_g   *?= 'Rofi'"
    ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 4;
      corner-radius = 0;
      round-borders = 0;

      rounded-corners-exclude = [
        "class_g = 'plptool-gui-PLPToolApp'",
        "class_g = 'dmenu'",
      ];
    '';
    fade = true;
    fadeDelta = 5;
    package = pkgs.picom.overrideAttrs (
      o: {
        src = pkgs.fetchFromGitHub {
          repo = "picom";
          owner = "ibhagwan";
          rev = "44b4970f70d6b23759a61a2b94d9bfb4351b41b1";
          sha256 = "0iff4bwpc00xbjad0m000midslgx12aihs33mdvfckr75r114ylh";
        };
      }
    );
  };

  home.file = {
    #".Xresources".text = builtins.readFile ./system/Xresources;
    ".config/xmobar/xmobarrc0".text = builtins.readFile ./system/xmobarrc0;
    ".config/xmobar/xmobarrc1".text = builtins.readFile ./system/xmobarrc1;
    ".config/xmobar/xmobarrc2".text = builtins.readFile ./system/xmobarrc2;
  };
}
