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

  configs = import ../../../config/dolphin/config.nix;

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
    configs);

in {
  options.programs.cory.dolphin = {
    enable = mkEnableOption "Enables dolphin";
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
  };
}
