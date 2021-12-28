{ config, pkgs, ... }:

{
  imports =
    [
      ./discord
      ./dunst
      ./emacs
      ./kitty
      ./nvim
      ./zathura
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
    ".config/xmobar/xmobarrc0".text = builtins.readFile ./xmobar/xmobarrc0;
    ".config/xmobar/xmobarrc1".text = builtins.readFile ./xmobar/xmobarrc1;
    ".config/xmobar/xmobarrc2".text = builtins.readFile ./xmobar/xmobarrc2;
    "Pictures/wallpaper.jpg".source = ./wallpapers/tree3.jpg;
  };

  home.packages = with pkgs; [
    arc-theme
    tela-icon-theme
    orchis-theme
    whitesur-icon-theme
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
