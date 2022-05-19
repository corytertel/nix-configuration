{ config, pkgs, ... }:

{
  imports =
    [
      ./dunst
      ./rofi
      ./tint2
      ./urxvt
      ./zathura
    ];

  # opacityRule = [
  #   "100:class_g   *?= 'FvwmButtons'"
  #   "100:class_g   *?= 'FvwmPager'"
  #   "100:class_g   *?= 'FvwmScript'"
  #   "100:class_g   *?= 'Rofi'"
  # ];
  # extraOptions = ''
  #     blur-method = "dual_kawase";
  #     blur-strength = 2;

  #     shadow-red = 0.8;
  #     shadow-green = 0.1412;
  #     shadow-blue = 0.1137;

  #     corner-radius = 20;
  #     round-borders = 1;
  #     rounded-corners-exclude = [
  #       "name *?= 'tint2'",
  #     ];
  #   '';
  # "widthb > 3830 && heightb > 2150",
  # blur-kern = "3x3box";
  # blur-background-exclude = [
  # ];
  # blur-background = true;
  # blur-background-frame = true;
  # blur-background-fixed = true;
  # shadow-radius = 12;
  # fade = true;
  # fadeDelta = 2;
  # shadowExclude = [
  #   "focused = 0"
  #   "class_g *?= 'FvwmButtons'"
  #   "class_g *?= 'FvwmPager'"
  #   "class_g *?= 'FvwmScript'"
  # ];

  services.picom = {
    enable = true;
    inactiveDim = "0.02";
    # experimentalBackends = true;
    experimentalBackends = false;
    backend = "glx";
    # paint-on-overlay = true;
    # clear-shadow = true;
    # vsync = "opengl-swc";
    vSync = true;
    extraOptions = ''
      glx-no-stencil = true;
      glx-no-rebind-pixmap = true;

      corner-radius = 10;
      round-borders = 1;

      shadow = true;
      shadow-radius = 40;
      shadow-opacity = 0.70;
      shadow-offset-x = -40;
      shadow-offset-y = -30;
      clip-shadow-above = [
        "class_g *?= 'FvwmButtons'",
        "class_g *?= 'FvwmPager'",
        "class_g *?= 'tint2'",
      ];

      wintypes:
      {
        dock = { shadow = true; };
        dnd = { shadow = true; };
      };
    '';
    # package = pkgs.picom.overrideAttrs (
    #   o: {
    #     src = pkgs.fetchFromGitHub {
    #       # owner = "Arian8j2";
    #       # repo = "picom-jonaburg-fix";
    #       # rev = "31d25da22b44f37cbb9be49fe5c239ef8d00df12";
    #       # sha256 = "1z4bKDoNgmG40y2DKTSSh1NCafrE1rkHkCB3ob8ibm4=";
    #       owner = "jonaburg";
    #       repo = "picom";
    #       rev = "e3c19cd7d1108d114552267f302548c113278d45";
    #       sha256 = "4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y=";
    #     };
    #   }
    # );
  };

  home.file = {
    ".fvwm" = {
      recursive = true;
      source = ./fvwm;
    };
  };

  home.packages = with pkgs; [
    libsForQt5.qtstyleplugins
  ];

  dconf.enable = true;
  gtk = {
    enable = true;

    font = {
      package = null;
      name = "NotoSans Nerd Font 10";
    };

    theme = {
      package = pkgs.plainlight-gtk-theme;
      name = "PlainLight";
    };

    iconTheme = {
      package = pkgs.tango-icon-theme;
      name = "Tango";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Tango";
      gtk-theme-name = "PlainLight";
      gtk-application-prefer-dark-theme = 0;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };
}
