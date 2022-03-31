{ config, pkgs, ... }:

{
  imports =
    [
      ./dunst
      ./urxvt
      ./zathura
    ];

  services.picom = {
    enable = true;
    inactiveOpacity = "1.00";
    activeOpacity = "1.00";
    # blur = true;
    experimentalBackends = true;
    # extraOptions = let
    #   decorations = "!name~=''";
    # in ''
    #   blur-method = "dual_kawase";
    #   blur-strength = 8;

    #   corner-radius = 0;
    #   round-borders = 1;
    #   rounded-corners-exclude = [
    #     "${decorations}",
    #     "name = 'xmobar'",
    #   ];

    #   shadow = true;
    #   noDockShadow = false;
    #   shadow-radius = 30;
    #   shadow-opacity = 0.95;
    #   shadow-offset-x = -25;
    #   shadow-offset-y = -23;
    # '';
    shadow = false;
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
    ".config/xmobar/xmobarrc".text = builtins.readFile ./xmobar/xmobarrc;
    ".config/xmobar/volume.sh".source = ./xmobar/volume.sh;
  };

  home.packages = with pkgs; [
    libsForQt5.qtstyleplugins
  ];

  dconf.enable = true;
  gtk = {
    enable = true;

    font = {
      package = null;
      name = "VictorMono Nerd Font";
    };

    theme = {
      package = pkgs.blacknord-gtk-theme;
      name = "BlackNord";
    };

    iconTheme = {
      package = pkgs.pop-icon-theme;
      name = "Pop";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Pop";
      gtk-theme-name = "BlackNord";
      gtk-application-prefer-dark-theme = 0;
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };
}
