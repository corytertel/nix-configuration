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
    apps.launcher = {
      name = "rofi";
      command = "rofi -show drun -modi drun,run -show -scroll-method 0 -sort -hover-select -me-select-entry '' -me-accept-entry MousePrimary";
      desktopFile = "rofi.desktop";
      package = pkgs.rofi;
    };
  };
}
