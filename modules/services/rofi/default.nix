{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.cory.rofi;
in {
  options.services.cory.rofi = {
    enable = mkEnableOption "Enables rofi";
    config = mkOption {
      type = with types; nullOr (oneOf [ str path ]);
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.programs.rofi = {
      enable = true;
      terminal = "${config.apps.terminal.package}/bin/${config.apps.terminal.command}";
      theme = cfg.config;
    };
  };
}
