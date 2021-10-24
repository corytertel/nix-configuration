{ config, pkgs, ... }:

{
  imports =
    [
      ./bash
      ./discord
      ./dunst
      ./kitty
      ./rofi
      ./zathura
    ];

  services.picom = {
    enable = true;
    inactiveOpacity = "1.0";
    activeOpacity = "1.0";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      "98:class_g   *?= 'emacs'"
      "98:class_g   *?= 'discord'"
      #"75:class_g   *?= 'Rofi'"
    ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 4;
      corner-radius = 40;
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
          repo = "picom";
          owner = "ibhagwan";
          rev = "44b4970f70d6b23759a61a2b94d9bfb4351b41b1";
          sha256 = "0iff4bwpc00xbjad0m000midslgx12aihs33mdvfckr75r114ylh";
        };
      }
    );
  };

  home.file = {
    ".config/xmobar/xmobarrc0".text = builtins.readFile ./xmobar/xmobarrc0;
    ".config/xmobar/xmobarrc1".text = builtins.readFile ./xmobar/xmobarrc1;
    ".config/xmobar/xmobarrc2".text = builtins.readFile ./xmobar/xmobarrc2;
    "Pictures/wallpaper.jpg".source = ./wallpaper.jpg;
  };

  home.packages = with pkgs; [
    whitesur-icon-theme
    orchis-theme
  ];

  gtk = {
    enable = true;

    iconTheme = {
      package = pkgs.whitesur-icon-theme;
      name = "WhiteSur-dark";
    };

    theme = {
      package = pkgs.orchis-theme;
      name = "orchis-dark-compact";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "WhiteSur-dark";
      gtk-theme-name = "orchis-dark-compact";
      gtk-application-prefer-dark-theme = 1;
    };
  };
}
