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
    enable = mkEnableOption "Enable fvwm";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      displayManager = {
        defaultSession = "none+xmonad";
        sddm.enable = true;
      };

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
        ];
        config = ./xmonad.hs;
      };
    };

    environment.variables = {
      QT_AUTO_SCREEN_SCALE_FACTOR = "0";
      PLASMA_USE_QT_SCALING = "1";
    };

    environment = {
      systemPackages = with pkgs; [
        feh
        xorg.xwd
        xlockmore
	      stalonetray
        flameshot
        pavucontrol
        pasystray
        networkmanagerapplet
        cbatticon
        xdgmenumaker
        xbrightness
        imagemagick
        kde-gtk-config
        trash-cli
        xdotool

        conky
        lua
        lm_sensors
        lsb-release

        # all configured in dconf
        mate.eom
        mate.caja
        mate.atril
        mate.mate-terminal
        mate.mate-system-monitor
        mate.mate-power-manager
        upower
        mate.mate-media
      ];
    };

    home-manager.users.cory.home.pointerCursor = with config.theme; {
      name = cursor.theme;
      size = cursor.size;
      gtk.enable = true;
      package = cursor.package;
      x11 = {
        enable = true;
        defaultCursor = "left_ptr";
      };
    };
  };
}
