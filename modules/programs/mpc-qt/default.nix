{ config, lib, pkgs, ...}:
with lib;

let
  cfg = config.programs.cory.mpc-qt;
in {
  options.programs.cory.mpc-qt = {
    enable = mkEnableOption "Enable mpc-qt";
  };

  config = mkIf cfg.enable {
    apps.videoPlayer = {
      name = "mpc-qt";
      command = "mpc-qt";
      desktopFile = "mpc-qt.desktop";
      package = pkgs.mpc-qt;
    };

    home-manager.users.cory.home.file.".config/mpc-qt/settings.json".text =
      builtins.readFile ./settings.json;
  };
}
