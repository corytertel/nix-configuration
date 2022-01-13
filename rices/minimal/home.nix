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
    blur = false;
    experimentalBackends = true;
    opacityRule = [
      "100:class_g   *?= 'FvwmButtons'"
      "100:class_g   *?= 'FvwmPager'"
      "100:class_g   *?= 'FvwmScript'"
      "100:class_g   *?= 'Rofi'"
    ];
    #  blur-method = "dual_kawase";
    #  blur-strength = 8;
    extraOptions = ''
      corner-radius = 45;
      round-borders = 1;

      rounded-corners-exclude = [
        "name != 'BottomDock' && name != 'Time' && name != 'Date' && name != 'Volume' && name != 'Battery' && name != 'ControlButtons'",
      ];
    '';
    shadow = true;
    shadowExclude = [
      "class_g *?= 'FvwmPager'"
      "class_g *?= 'FvwmScript'"
      "class_g *?= 'FvwmButtons'"
    ];
    vSync = true;
    package = pkgs.picom.overrideAttrs (
      o: {
        src = pkgs.fetchFromGitHub {
          owner = "jonaburg";
          repo = "picom";
          rev = "a8445684fe18946604848efb73ace9457b29bf80";
          sha256 = "R+YUGBrLst6CpUgG9VCwaZ+LiBSDWTp0TLt1Ou4xmpQ=";
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
    orchis-theme
    tela-icon-theme
    gruvbox-dark-gtk
    gruvbox-dark-icons-gtk
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
      name = "Tinos Nerd Font 11";
    };

    theme = {
      package = pkgs.orchis-theme;
      name = "Orchis-light";
    };

    iconTheme = {
      package = pkgs.tela-icon-theme;
      name = "Tela";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Tela";
      gtk-theme-name = "Orchis-light";
      gtk-application-prefer-dark-theme = 0;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };
}
