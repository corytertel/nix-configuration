{ config, pkgs, ... }:

{
  imports =
    [
      ./dunst
      ./rofi
      ./urxvt
      ./zathura
    ];

  services.picom = {
    enable = true;
    inactiveOpacity = "1.00";
    activeOpacity = "1.00";
    experimentalBackends = true;
    opacityRule = [
      # "100:class_g  *?= 'urxvt'"
      # "80:class_g   *?= 'Emacs'"
      # "80:class_g   *?= 'discord'"
      # "90:class_g   *?= 'Rofi'"
      # "90:!name      ~= ''"
    ];
    vSync = true;
    # package = pkgs.picom.overrideAttrs (
    #   o: {
    #     src = pkgs.fetchFromGitHub {
    #       owner = "Arian8j2";
    #       repo = "picom-jonaburg-fix";
    #       rev = "31d25da22b44f37cbb9be49fe5c239ef8d00df12";
    #       sha256 = "1z4bKDoNgmG40y2DKTSSh1NCafrE1rkHkCB3ob8ibm4=";
    #     };
    #   }
    # );
  };

  home.file = {
    ".config/xmobar/bar".text = builtins.readFile ./xmobar/bar;
    ".config/xmobar/vol_free.sh".source = ./xmobar/vol_free.sh;
    ".config/xmobar/vol_used.sh".source = ./xmobar/vol_used.sh;
    ".icons/icons" = {
      recursive = true;
      source = ./icons;
    };
  };

  home.packages = with pkgs; [
    orchis-theme
    #tela-icon-theme
    #gruvbox-dark-gtk
    zafiro-icons
    libsForQt5.qtstyleplugins
    #gsettings-desktop-schemas
    #gnome.gnome-themes-extra
  ];

  dconf.enable = true;
  gtk = {
    enable = true;

    font = {
      package = null;
      name = "Iosevka Nerd Font 11";
    };

    theme = {
      package = pkgs.orchis-theme;
      #name = "Orchis-green-light";
      name = "Orchis-light";
    };

    iconTheme = {
      package = pkgs.zafiro-icons;
      name = "Zafiro-icons";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Zafiro-icons";
      #gtk-theme-name = "Orchis-green-light";
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
