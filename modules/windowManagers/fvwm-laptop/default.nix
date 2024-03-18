{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.windowManagers.cory.fvwm.laptop;

  fvwm-config = pkgs.stdenv.mkDerivation {
    name = "fvwm-config";
    dontBuild = true;
    # buildInputs = [ pkgs.xdgmenumaker ];
    # buildPhase = ''
    #   xdgmenumaker -i -f fvwm -s 32 \
    #   | grep -v -G "Title$" \
    #   | sed s#'\+ "\(.*\)%.*\(48x48\|256x256\|512x512\).*"'#'+ "\1"'# \
    #   > xdgmenu
    # '';
    installPhase = ''
      cp -r $src $out
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
        defaultSession = "none+fvwm";
        sddm = {
          enable = true;
          enableHidpi = true;
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

      fvwm_img = "${fvwm-config}/images";
      fvwm_icon = "${config.theme.icons.package}/share/icons/crystal-nova";
      fvwm_wallpaper = "${fvwm-config}/images/wallpaper";
      fvwm_cache = "/tmp/.fvwm-cache";
      fvwm_scripts = "${fvwm-config}/scripts";

      fvwm_term = config.apps.terminal.command;
      fvwm_browser = config.apps.browser.command;
      fvwm_editor = config.apps.editor.command;
      fvwm_file_manager = config.apps.fileManager.command;
      fvwm_music_player = config.apps.musicPlayer.command;
      fvwm_video_player = config.apps.videoPlayer.command;
      fvwm_chat = "discord";
      fvwm_mail = "thunderbird";

      QT_AUTO_SCREEN_SCALE_FACTOR = "0";
      PLASMA_USE_QT_SCALING = "1";
    };

    environment = {
      systemPackages = with pkgs; [
        fvwm
        fvwm-config
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
  };
}
