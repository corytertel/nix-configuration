{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.windowManagers.cory.kde;
in {
  options.windowManagers.cory.kde = {
    enable = mkEnableOption "Enable kde";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      displayManager = {
        defaultSession = "plasma";
        sddm = {
          enable = true;
          enableHidpi = true;
        };
      };

      desktopManager.plasma5 = {
        enable = true;
        phononBackend = "vlc";
        useQtScaling = true;
      };
    };

    environment = {
      systemPackages = with pkgs; [];
    };
  };
}
