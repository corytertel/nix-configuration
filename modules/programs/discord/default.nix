{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.discord;
in {
  options.programs.cory.discord = {
    enable = mkEnableOption "Enables discord";
    css = mkOption {
      type = types.lines;
      default = import ../../../config/discocss/custom.nix { inherit config pkgs; };
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.cory = {
      home.packages = [ pkgs.discord ];
      xdg.configFile."discocss/custom.css".text = cfg.css;
    };
  };
}
