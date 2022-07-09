{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.cory.rofi;

  rofi-config = import ../../../config/rofi/krunner.nix { inherit config pkgs; };
in {
  options.services.cory.rofi = {
    enable = mkEnableOption "Enables rofi";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.programs.rofi = {
      enable = true;
      terminal = "${config.apps.terminal.package}/bin/${config.apps.terminal.command}";
      theme = "${rofi-config}";
    };
    apps.launcher = {
      name = "rofi";
      command = "rofi -show drun -modi drun,run -show-icons";
      desktopFile = "rofi.desktop";
      package = pkgs.rofi;
    };
  };
}
