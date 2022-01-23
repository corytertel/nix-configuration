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
    inactiveOpacity = "0.90";
    activeOpacity = "1.00";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      "100:class_g   *?= 'FvwmButtons'"
      "100:class_g   *?= 'FvwmPager'"
      "100:class_g   *?= 'FvwmScript'"
      "100:class_g   *?= 'Rofi'"
    ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 2;

      shadow-red = 0.8;
      shadow-green = 0.1412;
      shadow-blue = 0.1137;

      corner-radius = 20;
      round-borders = 1;
      rounded-corners-exclude = [
        "name *?= 'tint2'",
      ];
    '';
        # "widthb > 3830 && heightb > 2150",
      # blur-kern = "3x3box";
      # blur-background-exclude = [
      # ];
      # blur-background = true;
      # blur-background-frame = true;
      # blur-background-fixed = true;
      # shadow-radius = 12;
    fade = true;
    fadeDelta = 2;
    shadow = true;
    shadowExclude = [
      "focused = 0"
      "class_g *?= 'FvwmButtons'"
      "class_g *?= 'FvwmPager'"
      "class_g *?= 'FvwmScript'"
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
    zafiro-icons
    gruvbox-dark-gtk
    gruvbox-dark-icons-gtk
    libsForQt5.qtstyleplugins
    gsettings-desktop-schemas
    gnome.gnome-themes-extra
  ];

  dconf.enable = true;
  gtk = {
    enable = true;

    font = {
      package = null;
      name = "JetBrainsMono Nerd Font 10";
    };

    theme = {
      package = pkgs.gruvbox-dark-gtk;
      name = "gruvbox-dark";
    };

    iconTheme = {
      package = pkgs.zafiro-icons;
      name = "Zafiro-icons";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Zafiro-icons";
      gtk-theme-name = "gruvbox-dark";
      gtk-application-prefer-dark-theme = 1;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };
}
