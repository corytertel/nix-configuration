{ config, pkgs, ... }:

{
  imports =
    [
      ./discord
      ./dunst
      ./emacs
      ./kitty
      ./zathura
    ];

  services.picom = {
    enable = true;
    inactiveDim = "0.1";
    blur = true;
    experimentalBackends = true;
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 4;
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
      name = "Orchis-light-compact";
    };

    iconTheme = {
      package = pkgs.tela-icon-theme;
      name = "Tela";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Tela";
      gtk-theme-name = "Orchis-light-compact";
      gtk-application-prefer-dark-theme = 0;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };
}
