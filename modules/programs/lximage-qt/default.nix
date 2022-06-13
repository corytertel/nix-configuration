{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.lximage-qt;
in {
  options.programs.cory.lximage-qt = {
    enable = mkEnableOption "Enables lximage-qt";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory = {
      home.file.".config/lximage-qt/settings.conf".text =
        builtins.readFile ../../../config/lximage-qt/settings.conf;
      home.packages = [ pkgs.lxqt.lximage-qt ];
    };
  };
}
