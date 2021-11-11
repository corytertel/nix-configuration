{ config, pkgs, ... }:

{
  imports =
    [
      ./bash
      ./discord
      ./dunst
      ./kitty
      ./rofi
      ./zathura
    ];

  services.picom = {
    enable = true;
    inactiveOpacity = "0.93";
    activeOpacity = "0.98";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      #"98:class_g   *?= 'emacs'"
      #"98:class_g   *?= 'discord'"
    ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 4;
      corner-radius = 20;
      round-borders = 1;

      rounded-corners-exclude = [
        "class_g = 'dmenu'",
      ];
    '';
    fade = true;
    fadeDelta = 5;
    #shadow = true;
    #shadowOpacity = "0.5";
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
    ".config/xmobar/xmobarrc0".text = builtins.readFile ./xmobar/xmobarrc0;
    ".config/xmobar/xmobarrc1".text = builtins.readFile ./xmobar/xmobarrc1;
    ".config/xmobar/xmobarrc2".text = builtins.readFile ./xmobar/xmobarrc2;
    "Pictures/wallpaper.jpg".source = ./wallpapers/tree3.jpg;
  };

  home.packages = with pkgs; [
    arc-theme
    tela-icon-theme
    orchis-theme
    whitesur-icon-theme
    libsForQt5.qtstyleplugins
    gnome3.dconf
    gsettings-desktop-schemas
    gnome.gnome-themes-extra
  ];

  dconf.enable = true;
  gtk = {
    enable = true;

    font = {
      package = null;
      name = "FantasqueSansMono Nerd Font 11";
    };

    theme = {
      #package = pkgs.arc-theme;
      #name = "Arc-Dark";
      package = pkgs.orchis-theme;
      name = "Orchis-dark-compact";
    };

    iconTheme = {
      package = pkgs.tela-icon-theme;
      name = "Tela";
      #package = pkgs.whitesur-icon-theme;
      #name = "WhiteSur-dark";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Tela";
      #gtk-theme-name = "Arc-Dark";
      #gtk-icon-theme-name = "WhiteSur-dark";
      gtk-theme-name = "Orchis-dark-compact";
      gtk-application-prefer-dark-theme = 1;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };
}
