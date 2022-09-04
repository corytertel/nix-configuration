{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.cory.touchegg;
in {
  options.services.cory.touchegg = {
    enable = mkEnableOption "Enables touchegg";
    config = mkOption {
      type = types.nullOr types.path;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    services.touchegg.enable = true;
    home-manager.users.cory = {
      xdg.configFile."touchegg/touchegg.conf".text =
        builtins.readFile cfg.config;
    };
    home-manager.users.cory.home.file.".config/autostart/touchegg.desktop".source = ./touchegg.desktop;
  };
}
