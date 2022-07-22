{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.windowManagers.cory.exwm;
in {
  options.windowManagers.cory.exwm = {
    enable = mkEnableOption "Enables exwm";
    dpi = mkOption {
      type = types.int;
      default = 100;
    };
    hidpi = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {

    programs.cory.emacs = {
      enable = true;
      exwm = true;
    };

    services.xserver = {
      enable = true;
      updateDbusEnvironment = true;

      windowManager.session = let
        loadScript = pkgs.writeText "emacs-exwm-load" ''
          (setq mb/system-settings
            '((desktop/dpi . ${(toString cfg.dpi)})
              (desktop/hidpi . ${if cfg.hidpi then "t" else "nil"})))
        '';
      in singleton {
        name = "exwm";
        start = ''
          ${pkgs.dbus.dbus-launch} --exit-with-session emacs -mm --fullscreen \
              -l "${loadScript}"
        '';
      };

      displayManager.sddm = {
        enable = true;
        enableHidpi = cfg.hidpi;
      };
    };

    environment.systemPackages = with pkgs; [
      flameshot
      feh
    ];
  };
}
