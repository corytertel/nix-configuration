{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.windowManagers.cory.fvwm.laptop;

  fvwm-config = pkgs.stdenv.mkDerivation {
    name = "fvwm-config";
    dontBuild = true;
    installPhase = ''
      cp -r $src $out
    '';
    src = ../../../../config/fvwm/laptop;
  };

in {
  options.windowManagers.cory.fvwm.laptop = {
    enable = mkEnableOption "Enable fvwm";
  };

  imports = [
    ./kde.nix
  ];

  config = mkIf cfg.enable {
    services.xserver = {
      windowManager.session = [{
        name = "fvwm3";
        start = ''
        ${pkgs.fvwm3}/bin/fvwm3 &
        waitPID=$!
      '';
      }];
      displayManager.sessionCommands = ''
        # Allow local user to control X settings (specifically the monitor setup)
        ${pkgs.xorg.xhost}/bin/xhost si:localuser:root

        # Prevent screen from turning off
        ${pkgs.xorg.xset}/bin/xset s off -dpms

        # Fix horrible default key repeat delay in xorg-server-1.6
        ${pkgs.xorg.xset}/bin/xset r rate 200 25

        # Load system-wide Xresources
        ${pkgs.xorg.xrdb}/bin/xrdb -merge /etc/X11/Xresources
      '';
    };

    services.displayManager = {
      defaultSession = "none+fvwm3";
      sddm = {
        enable = true;
        enableHidpi = true;
        wayland.enable = false;
      };
    };

    # Compositor
    services.picom = {
      enable = true;
      shadow = true;
      shadowOffsets = [ (-15) (-15) ];
      shadowOpacity = 0.75;
      fade = true;
      fadeDelta = 5;
      settings = {
        fade-in-step = 1.0; # no fade for new windows
        inactive-dim = 0.02;
        focus-exclude = [
          "class_g = 'RightPanel'"
        ];
      };
    };

    # Auto detect and configure new monitors
    # Allow users of the "video" group to change brightness
    services.udev.extraRules = let
      defaultMonitor = "eDP-1";
      defaultResolution = "2256x1504";
      # for some reason Nix puts Xauthority in /tmp with a random path, so we must find and set it
      script = pkgs.writeShellScript "hotplug_monitor.sh" ''
        export DISPLAY=:0
        export XAUTHORITY=$(${pkgs.coreutils-full}/bin/ls /tmp | ${pkgs.gnugrep}/bin/grep xauth | ${pkgs.coreutils-full}/bin/head -n1)

        function connect() {
          ${pkgs.xorg.xrandr}/bin/xrandr --output DP-4 --auto --right-of ${defaultMonitor}
          ${pkgs.xorg.xrandr}/bin/xrandr --output DP-4 --auto --scale-from ${defaultResolution} --same-as ${defaultMonitor}
        }

        function disconnect() {
          ${pkgs.xorg.xrandr}/bin/xrandr --output DP-4 --off
        }

        ${pkgs.xorg.xrandr}/bin/xrandr | grep "DP-4 connected" &> /dev/null && connect || disconnect
      '';
    in ''
      ACTION=="change", SUBSYSTEM=="drm", RUN+="${script}"

      ACTION=="add", SUBSYSTEM=="backlight", RUN+="${pkgs.coreutils-full}/bin/chgrp video $sys$devpath/brightness", RUN+="${pkgs.coreutils-full}/bin/chmod g+w $sys$devpath/brightness"
    '';

    environment.variables = let
      fvwm-path = "/usr/local/share/fvwm";
    in {
      # Link so the fvwm config can be hot-reloaded
      FVWM_DATADIR = fvwm-path;
      FVWM_USERDIR = fvwm-path;

      fvwm_img = "${fvwm-path}/images";
      fvwm_icon = "${config.theme.icons.package}/share/icons/crystal-nova";
      fvwm_wallpaper = "${fvwm-path}/images/background";
      fvwm_cache = "/tmp/.fvwm-cache";
      fvwm_scripts = "${fvwm-path}/scripts";

      fvwm_term = config.apps.terminal.command;
      fvwm_browser = config.apps.browser.command;
      fvwm_editor = config.apps.editor.command;
      fvwm_file_manager = config.apps.fileManager.command;
      fvwm_music_player = config.apps.musicPlayer.command;
      fvwm_video_player = config.apps.videoPlayer.command;
      fvwm_chat = "discord";
      fvwm_mail = "thunderbird";

      # QT_AUTO_SCREEN_SCALE_FACTOR = "0";
      # PLASMA_USE_QT_SCALING = "1";
    };

    # Link out fvwm to the expected place of /usr/local/share/fvwm/config
    system.activationScripts.fvwm.text = ''
      mkdir -p /usr/local/share
      chmod 0755 /usr
      chmod 0755 /usr/local
      chmod 0755 /usr/local/share
      ln -sfn ${fvwm-config} /usr/local/share/fvwm
    '';

    environment.systemPackages = with pkgs; [
      fvwm3
      fvwm-config
      feh
      xorg.xwd
      xlockmore
      xscreensaver
	    stalonetray
      flameshot
      pavucontrol
      pasystray
      networkmanagerapplet
      cbatticon
      (xdgmenumaker.overridePythonAttrs (old: rec {
        pythonPath = with python3Packages; [
          pygobject3
          pyxdg
          pillow
        ];
      }))
      acpilight
      imagemagick
      trash-cli
      xdotool
      pamixer
      arandr
      playerctl
      rofi
    ];
    # } // import ./kde.nix { inherit config lib pkgs; };
};
}
