{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.cory.libinput-gestures;
  kwinShortcut = shortcut:
    "qdbus org.kde.kglobalaccel /component/kwin invokeShortcut '${shortcut}'";
in {
  options.services.cory.libinput-gestures = {
    enable = mkEnableOption "Enables gestures";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory = {
      systemd.user.services = {
        libinput-gestures = {
          Unit = {
            Description = "Set libinput-gestures service";
            After = [ "graphical-session-pre.target" ];
            PartOf = [ "graphical-session.target" ];
          };

          Install = { WantedBy = [ "graphical-session.target" ]; };

          Service = {
            Type = "simple";
            RemainAfterExit = "yes";
            ExecStart = "${pkgs.libinput-gestures}/bin/libinput-gestures";
          };
        };
      };

      home.file.".config/libinput-gestures.conf".text = ''
        # KDE: Present Windows (current desktop)
        gesture swipe left 3 ${kwinShortcut "Expose"}

        # KDE: Present Windows (Window class)
        gesture swipe right 3 ${kwinShortcut "ExposeAll"}

        # KDE: Switch to Workspace Above
        gesture swipe down 3 ${kwinShortcut "Switch One Desktop Up"}

        # KDE: Switch to Workspace Below
        gesture swipe up 3 ${kwinShortcut "Switch One Desktop Down"}

        # KDE: Show desktop
        gesture pinch out ${kwinShortcut "Show Desktop"}
      '';

      home.packages = with pkgs; [
        libinput-gestures
      ];
    };
  };
}
