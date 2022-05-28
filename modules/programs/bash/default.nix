{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.bash;
in {
  options.programs.cory.bash = {
    enable = mkEnableOption "Enables bash";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.home.file = {
      ".bashrc".text = builtins.readFile ./bashrc;
    };
  };
}
