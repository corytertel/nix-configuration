{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.apps;
  mkApp = type: def: {
    name = mkOption{
      type = types.str;
      default = def;
    };
    command = mkOption {
      type = types.str;
      default = config.apps."${type}".name;
    };
    package = mkOption {
      type = types.nullOr types.package;
      default = pkgs."${config.apps."${type}".name}";
    };
    desktopFile = mkOption {
      type = types.str;
      default = "${config.apps."${type}".name}.desktop";
    };
  };
in {
  options.apps = with pkgs; {
    terminal = mkApp "terminal" "xterm";
    browser = mkApp "browser" "firefox";
    editor = mkApp "editor" "nano";
    photoViewer = mkApp "photoViewer" "sxiv";
    musicPlayer = mkApp "musicPlayer" "mpd";
    videoPlayer = mkApp "videoPlayer" "mpv";
    fileManager = mkApp "fileManager" "pcmanfm";
    pdfViewer = mkApp "pdfViewer" "zathura";
  };

  config = {
    environment.systemPackages = [
      cfg.terminal.package
      cfg.browser.package
      cfg.editor.package
      cfg.photoViewer.package
      cfg.musicPlayer.package
      cfg.videoPlayer.package
      cfg.fileManager.package
      cfg.pdfViewer.package
    ];
  };
}
