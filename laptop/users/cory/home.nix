{ config, pkgs, ... }:

{
  imports =
    [
      ./apps/kitty
      ./apps/bash
    ];

  # Xresources
    # DPI settings
    # Set DPI to the scale you want your applications at
    # 175 for desktop, 250 for laptop typically
  xresources.extraConfig = ''
    Xft.dpi: 250
  '';

  services.picom = {
    enable = true;
    inactiveOpacity = "0.50";
    activeOpacity = "0.90";
    blur = true;
    experimentalBackends = true;
    opacityRule = [
      "100:class_g   *?= 'Google-chrome'"
      "95:class_g   *?= 'Deadd-notification-center'"
      "75:class_g   *?= 'Rofi'"
    ];
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 8;
      corner-radius = 16;
      round-borders = 1;
        
      rounded-corners-exclude = [
        "class_g = 'Polybar'",
      ];
    '';
    fade = false;
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
    #".Xresources".text = builtins.readFile ./system/Xresources;
    ".config/xmobar/xmobarrc".text = builtins.readFile ./system/xmobarrc;
  };
}
