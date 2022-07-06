{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.qpdfview;
in {
  options.programs.cory.qpdfview = {
    enable = mkEnableOption "Enables qpdfview";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory = {
      # xdg.configFile."qpdfview/shortcuts.conf".text =
      #   builtins.readFile ../../../config/qpdfview/shortcuts.conf;
      home.file.".config/qpdfview/shortcuts.conf".text =
        builtins.readFile ../../../config/qpdfview/shortcuts.conf;
    };
    apps.pdfViewer = {
      name = "qpdfview";
      command = "qpdfview";
      desktopFile = "qpdfview.desktop";
      package = pkgs.qpdfview;
    };
  };
}
