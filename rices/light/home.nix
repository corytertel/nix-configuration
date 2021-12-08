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
    inactiveDim = "0.05";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      #"85:class_g   *?= 'emacs'"
      #"85:class_g   *?= 'discord'"
    ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 0;
    '';
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
    ".config/xmobar/xmobarrc".text = builtins.readFile ./xmobarrc;
    "Pictures/wallpaper.jpg".source = ./wallpapers/swirls.jpg;
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
