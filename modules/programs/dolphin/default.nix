{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.dolphin;

  dagEntryAfter = after: data: {
    inherit data after;
    before = [ ];
  };

  toValue = v:
    if builtins.isString v then
      v
    else if builtins.isBool v then
      boolToString v
    else if builtins.isInt v then
      builtins.toString v
    else
      builtins.abort ("Unknown value type: " ++ builtins.toString v);

  lines = flatten (mapAttrsToList
    (file:
      mapAttrsToList
        (group:
          mapAttrsToList
            (key: value:
              "$DRY_RUN_CMD ${pkgs.libsForQt5.kconfig}/bin/kwriteconfig5 --file '${file}' --group '${group}' --key '${key}' '${
                toValue value
              }'")
        ))
    cfg.config);

in {
  options.programs.cory.dolphin = {
    enable = mkEnableOption "Enables dolphin";
    config = mkOption {
      type = with types; nullOr (attrsOf (attrsOf (attrsOf (either bool (either int str)))));
      default = null;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.home.activation.dolphin = dagEntryAfter [ "linkGeneration" ] ''
      _() {
        ${builtins.concatStringsSep "\n" lines}
      } && _
      unset -f _
   '';

    apps.fileManager = {
      name = "dolphin";
      command = "dolphin --new-window";
      desktopFile = "org.kde.dolphin.desktop";
      package = pkgs.libsForQt5.dolphin;
    };

    home-manager.users.cory.home.file = {
      ".local/share/kxmlgui5/dolphin/dolphinui.rc".source = ./dolphinui.rc;
      ".local/share/user-places.xbel".source = ./user-places.xbel;
    };

    environment.variables = {
      GTK_USE_PORTAL = "1";
      XDG_DESKTOP_PORTAL = "1";
    };

    xdg.portal = {
      enable = true;
      extraPortals = [ pkgs.libsForQt5.xdg-desktop-portal-kde ];
    };

    home-manager.users.cory.home.packages = with pkgs; [
      findutils
      libsForQt5.kio-extras
      libsForQt5.kget
      kompare # or kdiff3
      krename
      thunderbird
      gnutar
      gzip
      bzip2
      xz
      zip
      unzip
      rar
      rpm
      dpkg
      arj
      lha
      p7zip
      libsForQt5.ffmpegthumbs
    ];
  };
}
