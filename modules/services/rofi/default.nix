{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.cory.rofi;

  rofi-config = import ../../../config/rofi/config.nix { inherit pkgs; };
in {
  options.services.cory.rofi = {
    enable = mkEnableOption "Enables rofi";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.programs.rofi = {
      enable = true;
      terminal = "${pkgs.rxvt-unicode}/bin/urxvtc";
      theme = "${rofi-config}";
    };
  };
}
