{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.firefox;
in {
  options.programs.cory.firefox = {
    enable = mkEnableOption "Enables firefox";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.programs.firefox = {
      enable = true;
      profiles."cory" = {
        bookmarks = import ./bookmarks.nix;
        settings = import ./settings.nix { inherit config lib; };
        extensions = import ./extensions.nix { inherit pkgs; };
      };
    };
    apps.browser = {
      name = "firefox";
      command = "firefox";
      desktopFile = "firefox.desktop";
      package = pkgs.firefox;
    };
  };
}
