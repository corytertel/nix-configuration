{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.windowManagers.cory.fvwm.pc;

  fvwm-config = pkgs.stdenv.mkDerivation {
    name = "fvwm-config";
    dontBuild = true;
    installPhase = ''
      cp -aR $src $out
    '';
    src = ../../../config/fvwm-pc;
  };

in {
  options.windowManagers.cory.fvwm.pc = {
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
      fvwm_cache = "/tmp/fvwm-cache";
      fvwm_scripts = "${fvwm-config}/scripts";

      fvwm_term = config.apps.terminal.command;
      fvwm_browser = config.apps.browser.command;
      fvwm_editor = config.apps.editor.command;
      fvwm_file_manager = config.apps.fileManager.command;
      fvwm_music_player = config.apps.musicPlayer.command;
      fvwm_video_player = config.apps.videoPlayer.command;
      fvwm_launch = config.apps.launcher.command;
      fvwm_chat = "discord";
      fvwm_mail = "thunderbird";

      QT_AUTO_SCREEN_SCALE_FACTOR = "0";
      PLASMA_USE_QT_SCALING = "1";
    };

    security.pam.services.cory.enableKwallet = true;

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
        xdgmenumaker
        xbrightness
        imagemagick
        kde-gtk-config
        trash-cli
        xdotool
        xbrightness

        conky
        lua
        lm_sensors
        lsb-release

        libsForQt5.kwallet
        libsForQt5.kwallet-pam
        libsForQt5.kwalletmanager
        libsForQt5.plasma-systemmonitor
        libsForQt5.kirigami2
        libsForQt5.kirigami-addons
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
