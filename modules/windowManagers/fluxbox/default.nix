{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.windowManagers.cory.fluxbox;

  # fvwm-config = pkgs.stdenv.mkDerivation {
  #   name = "fvwm-config";
  #   dontBuild = true;
  #   installPhase = ''
  #     cp -aR $src $out
  #   '';
  #   src = ../../../config/fvwm-pc;
  # };

in {
  options.windowManagers.cory.fluxbox = {
    enable = mkEnableOption "Enable fluxbox";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      displayManager = {
        defaultSession = "none+fluxbox";
        sddm = {
          enable = true;
          enableHidpi = true;
        };
      };

      windowManager.session = [{
        name = "fluxbox";
        start = ''
          ${pkgs.fluxbox}/bin/startfluxbox &
          waitPID=$!
        '';
      }];
    };

    environment.variables = {
      QT_AUTO_SCREEN_SCALE_FACTOR = "0";
      PLASMA_USE_QT_SCALING = "1";
    };

    environment = {
      systemPackages = with pkgs; [
        fluxbox
        # fvwm-config
        menumaker
        feh
        flameshot
      ];
    };
  };
}
