{ config, pkgs, ... }:

{
  imports =
    [
      ./dunst
      ./rofi
      ./urxvt
      ./xmobar
      ./zathura
    ];

  services.picom = {
    enable = true;
    inactiveOpacity = "1.00";
    activeOpacity = "1.00";
    blur = true;
    experimentalBackends = true;
      # backend = "glx";
      # glx-no-stencil = true;
      # glx-copy-from-front = false;
      # glx-no-rebind-pixmap = true;
      # spawn-center-screen = true;
    extraOptions = ''
      shadow-radius = 10;
    '';
    shadow = true;
    shadowOffsets = [ 0 0 ];
    shadowOpacity = "0.60";
    # noDockShadow = false;
    # vSync = true;
    package = pkgs.picom.overrideAttrs (
      o: {
        src = pkgs.fetchFromGitHub {
          owner = "jonaburg";
          repo = "picom";
          rev = "e3c19cd7d1108d114552267f302548c113278d45";
          sha256 = "4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y=";
        };
      }
    );
  };

  home.packages = with pkgs; [
    libsForQt5.qtstyleplugins
  ];

  dconf.enable = true;
  gtk = {
    enable = true;

    font = {
      # package = pkgs.mplus-outline-fonts;
      # name = "M+ 1c 10";
      package = pkgs.nerdfonts;
      name = "mplus Nerd Font,M+ 1c 10";
    };

    theme = {
      package = pkgs.plainlight-gtk-theme;
      name = "PlainLight";
    };

    iconTheme = {
      # package = pkgs.luna-icons;
      # name = "Luna";
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
