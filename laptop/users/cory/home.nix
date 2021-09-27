{ config, pkgs, ... }:

{
  imports =
    [
      ./apps/kitty
      ./apps/bash
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
  
  services.picom = {
    enable = true;
    inactiveOpacity = "0.90";
    activeOpacity = "1.00";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      "100:class_g   *?= 'Google-chrome'"
      "95:class_g   *?= 'Deadd-notification-center'"
      "75:class_g   *?= 'Rofi'"
    ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 4;
      corner-radius = 32;
      round-borders = 1;
        
      rounded-corners-exclude = [
        "class_g = 'plptool-gui-PLPToolApp'",
      ];
    '';
    fade = false;
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
    ".config/xmobar/xmobarrc".text = builtins.readFile ./system/xmobarrc;
  };
}
