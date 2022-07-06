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
          # theme = "mountain-light";
        };
      };

      desktopManager.plasma5 = {
        enable = true;
        phononBackend = "vlc";
        useQtScaling = true;
      };
    };

    programs.dconf.enable = true;
    home-manager.users.cory = import ./config.nix { inherit config lib pkgs; };

    environment = {
      systemPackages = with pkgs; [
        # sddm-mountain-light
        libsForQt5.qt5.qttools
        krunner-desktop
      ];
    };
  };
}
