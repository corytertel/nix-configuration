{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.cory.touchegg;
in {
  options.services.cory.touchegg = {
    enable = mkEnableOption "Enables touchegg";
  };

  config = mkIf cfg.enable {
    services.touchegg.enable = true;
    home-manager.users.cory = {
      xdg.configFile."touchegg/touchegg.conf".text =
        builtins.readFile ../../../config/touchegg/touchegg.conf;
    };
  };
}
