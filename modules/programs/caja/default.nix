{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.caja;
in {
  options.programs.cory.caja = {
    enable = mkEnableOption "Enables caja";
  };

  config = mkIf cfg.enable {
    environment.variables = {
      GTK_USE_PORTAL = "1";
      XDG_DESKTOP_PORTAL = "1";
    };

    # xdg.portal = {
    #   enable = true;
    #   config.common.default = "*";
    #   extraPortals = [ pkgs.libsForQt5.xdg-desktop-portal-kde ];
    # };

    home-manager.users.cory.dconf = {
      enable = true;
      # settings = {
      #   "org/mate/caja/icon-view" = {
      #     default-use-tighter-layout = true;
      #     default-zoom-level = "larger";
      #     labels-beside-icons = false;
      #   };
      #   "org/mate/caja/list-view" = {
      #     default-column-order = ["name" "size" "type" "date_modified" "date_accessed" "date_created" "extension" "group" "where" "mime_type" "octal_permissions" "owner" "permissions" "size_on_disk"];
      #     default-visible-columns = ["name" "size" "type" "date_modified"];
      #     default-zoom-level = "smaller";
      #   };
      #   "org/mate/caja/preferences" = {
      #     always-use-location-entry = false;
      #     click-policy = "double";
      #   };
      #   "org/mate/caja/geometry" = {
      #     side-pane-view = "tree";
      #     start-with-location-bar = true;
      #     start-with-sidebar = true;
      #     start-with-status-bar = true;
      #     start-with-toolbar = true;
      #   };
      # };
    };

    apps.fileManager = {
      name = "caja";
      command = "caja";
      desktopFile = "caja.desktop";
      package = pkgs.mate.caja;
    };
  };
}
