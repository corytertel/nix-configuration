{ config, pkgs, ... }:

{
  imports =
    [
      ./discord
      ./dunst
      ./emacs
      ./nvim
      ./rofi
      ./urxvt
      ./zathura
    ];

  services.picom = {
    enable = true;
    inactiveOpacity = "0.93";
    activeOpacity = "1.00";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      "100:class_g   *?= 'FvwmButtons'"
      "100:class_g   *?= 'FvwmPager'"
      "100:class_g   *?= 'FvwmScript'"
    ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 8;

      rounded-corners-exclude = [
        "class_g = 'dmenu'",
      ];
    '';
    fade = false;
    fadeDelta = 5;
    shadow = true;
    shadowExclude = [
      "focused = 0"
    ];
    vSync = true;
    package = pkgs.picom.overrideAttrs (
      o: {
        src = pkgs.fetchFromGitHub {
          owner = "Arian8j2";
          repo = "picom-jonaburg-fix";
          rev = "31d25da22b44f37cbb9be49fe5c239ef8d00df12";
          sha256 = "1z4bKDoNgmG40y2DKTSSh1NCafrE1rkHkCB3ob8ibm4=";
        };
      }
    );
  };

  home.file = {
    ".fvwm" = {
      recursive = true;
      source = ./fvwm;
    };
  };

  home.packages = with pkgs; [
    luna-icons
    libsForQt5.breeze-gtk
    libsForQt5.qtstyleplugins
    #gnome3.dconf
    gsettings-desktop-schemas
    gnome.gnome-themes-extra
  ];

  dconf.enable = true;
  gtk = {
    enable = true;

    font = {
      package = null;
      name = "TerminessTTF Nerd Font 11";
    };

    theme = {
      package = pkgs.libsForQt5.breeze-gtk;
      name = "Breeze";
    };

    iconTheme = {
      package = pkgs.luna-icons;
      name = "Luna";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Luna";
      gtk-theme-name = "Breeze";
      gtk-application-prefer-dark-theme = 0;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };
}
