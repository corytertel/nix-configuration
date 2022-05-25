{ config, lib, pkgs, ... }:

let
  fvwm-config = pkgs.stdenv.mkDerivation {
    name = "fvwm-config";
    dontBuild = true;
    installPhase = ''
      cp -aR $src $out
    '';
    src = ../../../config/fvwm-pc;
  };
in {
  imports = [
    ../../bash
    ../../dunst
    # ../../firefox
    ../../gtk
    ../../layout_switch
    ../../neofetch
    ../../picom
    ../../plank
    ../../rofi
    ../../tint2
    ../../ungoogled-chromium
    ../../urxvt
    ../../zathura
    ../../zsh
  ];

  services.xserver = {
    displayManager = {
      defaultSession = "none+fvwm";
      sddm = {
        enable = true;
        enableHidpi = true;
        theme = "mountain-light";
      };
    };

    windowManager.session = [{
      name = "fvwm";
      # start = ''
      #   ${pkgs.fvwm}/bin/fvwm &
      #   waitPID=$!
      # '';
      start = ''
        ${pkgs.fvwm}/bin/fvwm -f ${fvwm-config}/config &
        waitPID=$!
      '';
    }];
  };

  environment.variables = { FVWM_USERDIR = "${fvwm-config}"; };

  environment = {
    systemPackages = with pkgs; [
      fvwm
      fvwm-config
      feh
      xorg.xwd
      sddm-mountain-light
      flameshot
    ];
  };
}
