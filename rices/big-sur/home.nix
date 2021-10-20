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
    inactiveOpacity = "1.00";
    activeOpacity = "1.00";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      "98:class_g   *?= 'emacs'"
      "98:class_g   *?= 'discord'"
    ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 4;
      corner-radius = 30;
      round-borders = 1;

      rounded-corners-exclude = [
        "class_g = 'plptool-gui-PLPToolApp'",
        "class_g = 'dmenu'",
      ];
    '';
    fade = true;
    fadeDelta = 5;
    shadow = false;
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
    ".config/xmobar/xmobarrc".text = builtins.readFile ./xmobar/xmobarrc;
    "Pictures/wallpaper.jpg".source = ./wallpaper.jpg;
  };
}
