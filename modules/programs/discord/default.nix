{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.discord;
in {
  options.programs.cory.discord = {
    enable = mkEnableOption "Enables discord";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.home.file = {
      ".config/discocss/custom.css".text = import ../../../config/discocss/custom.nix { inherit config pkgs; };
    };
    home-manager.users.cory.home.packages = [ pkgs.discord ];
  };
}
