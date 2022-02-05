{ config, pkgs, ... }:

{
  imports =
    [
      ./dunst
      ./emacs
      ./nvim
      ./rofi
      ./urxvt
      ./zathura
    ];

  services.picom = {
    enable = true;
    inactiveOpacity = "1.00";
    activeOpacity = "1.00";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      # "100:class_g  *?= 'urxvt'"
      # "90:class_g   *?= 'Emacs'"
      # "80:class_g   *?= 'discord'"
      # "80:class_g   *?= 'Rofi'"
      # "90:!name      ~= ''"
    ];
      # transition-length = 300
      # transition-pow-x = 0.1
      # transition-pow-y = 0.1
      # transition-pow-w = 0.1
      # transition-pow-h = 0.1
      # size-transition = true
      # corner-radius = 30;
      # round-borders = 1;
      # rounded-corners-exclude = [
      #   "name != 'xmobar'",
      # ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 8;

      shadow-radius = 30;
    '';
    fade = true;
    fadeDelta = 3;
    shadow = true;
    shadowOpacity = "0.90";
    noDockShadow = true;
    shadowExclude = [
      #"!name ~= ''"
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
    ".config/xmobar/launcher".text = builtins.readFile ./xmobar/launcher;
    ".config/xmobar/workspaces".text = builtins.readFile ./xmobar/workspaces;
    ".config/xmobar/dock".text = builtins.readFile ./xmobar/dock;
    ".config/xmobar/widgets".text = builtins.readFile ./xmobar/widgets;
    ".config/xmobar/clock".text = builtins.readFile ./xmobar/clock;
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
      name = "M+ 1c 10";
    };

    theme = {
      package = pkgs.orchis-theme;
      name = "Orchis-red-light";
    };

    iconTheme = {
      package = pkgs.zafiro-icons;
      name = "Zafiro-icons";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Zafiro-icons";
      gtk-theme-name = "Orchis-red-light";
      gtk-application-prefer-dark-theme = 0;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };
}
