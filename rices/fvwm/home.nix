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
    inactiveOpacity = "0.93";
    activeOpacity = "0.98";
    blur = true;
    experimentalBackends = true;
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 4;
      corner-radius = 10;
      round-borders = 1;

      rounded-corners-exclude = [
        "class_g = 'dmenu'",
      ];
    '';
    fade = true;
    fadeDelta = 5;
    shadow = true;
    shadowExclude = [
      "class_g   *?= 'Xclock'"
    ];
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
    luna-icons
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
