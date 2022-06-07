{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.windowManagers.cory.fvwm.laptop;

  fvwm-config = pkgs.stdenv.mkDerivation {
    name = "fvwm-config";
    dontBuild = true;
    installPhase = ''
      cp -aR $src $out
    '';
    src = ../../../config/fvwm-laptop;
  };

in {
  options.windowManagers.cory.fvwm.laptop = {
    enable = mkEnableOption "Enable fvwm-laptop rice";
  };

  config = mkIf cfg.enable {
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
        start = ''
          ${pkgs.fvwm}/bin/fvwm -f ${fvwm-config}/config &
          waitPID=$!
        '';
      }];
    };

    environment.variables = {
      FVWM_DATADIR = "${fvwm-config}";
      FVWM_USERDIR = "${fvwm-config}";
    };

    environment = {
      systemPackages = with pkgs; [
        fvwm
        fvwm-config
        feh
        xorg.xwd
        sddm-mountain-light
      ] ++ import ../wm-pkgs.nix { inherit pkgs; };
    };
  };



}
