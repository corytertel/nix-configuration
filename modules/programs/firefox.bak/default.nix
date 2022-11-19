{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.firefox;
in {
  options.programs.cory.firefox = {
    enable = mkEnableOption "Enables firefox";
    changeColor = mkOption {
      type = types.bool;
      default = true;
    };
    invertedColor = mkOption {
      type = types.str;
      default = config.theme.color.foreground;
    };
    secondaryColor = mkOption {
      type = types.str;
      default = config.theme.color.background-alt1;
    };
    windowColor = mkOption {
      type = types.str;
      default = config.theme.color.background;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.programs.firefox = {
      enable = true;
      extensions = import ../../../config/firefox/extensions.nix { inherit pkgs; };
      profiles."cory" = {
        userChrome = let
          changeColor = cfg.changeColor;
          invertedColor = cfg.invertedColor;
          secondaryColor = cfg.secondaryColor;
          windowColor = cfg.windowColor;
        in import ../../../config/firefox/userChrome.nix { inherit changeColor config invertedColor secondaryColor windowColor; };
        bookmarks = import ../../../config/firefox/bookmarks.nix;
        settings = import ../../../config/firefox/settings.nix { inherit config; };
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
