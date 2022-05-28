{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.layout_switch;
in {
  options.programs.cory.layout_switch = {
    enable = mkEnableOption "Enables layout_switch";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.home.file = {
      "manual_installs/layout_switch.sh".source = ./layout_switch.sh;
    };
  };
}
