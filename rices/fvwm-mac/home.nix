{ config, pkgs, ... }:

{
  imports =
    [
      ./discord
      ./dunst
      ./emacs
      ./kitty
      ./nvim
      ./rofi
      ./zathura
    ];

  services.picom = {
    enable = true;
    #inactiveDim = "0.01";
    inactiveOpacity = "1.00";
    activeOpacity = "1.00";
    blur = false;
    experimentalBackends = true;
    opacityRule = [
      "70:class_g   *?= 'XClock'"
    ];
    #extraOptions = ''
    #  blur-method = "dual_kawase";
    #  blur-strength = 4;
    #'';
    fade = true;
    fadeDelta = 5;
    shadow = true;
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
    "Pictures/wallpaper.jpg".source = ./wallpapers/asuka98.jpg;

    ".fvwm" = {
      recursive = true;
      source = ./fvwm;
    };
  };

  home.packages = with pkgs; [
    whitesur-icon-theme
    libsForQt5.breeze-gtk
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
      package = pkgs.libsForQt5.breeze-gtk;
      name = "Breeze";
    };

    iconTheme = {
      package = pkgs.whitesur-icon-theme;
      name = "WhiteSur";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "WhiteSur";
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
