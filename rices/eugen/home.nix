{ config, pkgs, ... }:

{
  imports =
    [
    ];

  services.picom = {
    enable = true;
    inactiveOpacity = "0.93";
    activeOpacity = "0.98";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      #"98:class_g   *?= 'emacs'"
      #"98:class_g   *?= 'discord'"
    ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 4;
      corner-radius = 20;
      round-borders = 1;

      rounded-corners-exclude = [
        "class_g = 'dmenu'",
        "class_g = 'xmobar'",
      ];
    '';
    fade = true;
    fadeDelta = 5;
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
    ".config/xmobar/xmobarrc".text = builtins.readFile ./xmobarrc;
    "Pictures/wallpaper.jpg".source = ./wallpapers/eugen-mountain.jpg;
  };

  home.packages = with pkgs; [
    tela-icon-theme
    orchis-theme
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
      name = "FantasqueSansMono Nerd Font 11";
    };

    theme = {
      package = pkgs.orchis-theme;
      name = "Orchis-dark-compact";
    };

    iconTheme = {
      package = pkgs.tela-icon-theme;
      name = "Tela red dark";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Tela red dark";
      gtk-theme-name = "Orchis-dark-compact";
      gtk-application-prefer-dark-theme = 1;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };
}
