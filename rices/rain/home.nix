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
    '';
    shadow = true;
    #shadowExclude = [
    #  "class_g   *?= 'Xclock'"
    #];
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
      name = "Orchis-dark";
    };

    iconTheme = {
      package = pkgs.tela-icon-theme;
      name = "Tela";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Tela";
      gtk-theme-name = "Orchis-dark";
      gtk-application-prefer-dark-theme = 1;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };
}
