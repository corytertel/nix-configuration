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
    blur = true;
    experimentalBackends = true;
    extraOptions = ''
      backend = "glx";
      glx-no-stencil = true;
      glx-copy-from-front = false;
      glx-no-rebind-pixmap = true;

      shadow-radius = 28;

      spawn-center-screen = true;
    '';
    shadow = true;
    # shadowOffsets = [ 0 0 ];
    shadowOpacity = "0.50";
    noDockShadow = false;
    # vSync = true;
    package = pkgs.picom.overrideAttrs (
      o: {
        src = pkgs.fetchFromGitHub {
          # owner = "Arian8j2";
          # repo = "picom-jonaburg-fix";
          # rev = "31d25da22b44f37cbb9be49fe5c239ef8d00df12";
          # sha256 = "1z4bKDoNgmG40y2DKTSSh1NCafrE1rkHkCB3ob8ibm4=";
          owner = "jonaburg";
          repo = "picom";
          rev = "e3c19cd7d1108d114552267f302548c113278d45";
          sha256 = "4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y=";
        };
      }
    );
  };

  home.file = {
    ".config/xmobar/xmobarrc".text = builtins.readFile ./xmobar/xmobarrc;
    ".config/xmobar/volume.sh".source = ./xmobar/volume.sh;
    ".config/xmobar/battery.sh".source = ./xmobar/battery.sh;
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
      package = pkgs.luna-icons;
      name = "Luna";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Luna";
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
