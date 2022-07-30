{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.gwenview;

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
  options.programs.cory.gwenview = {
    enable = mkEnableOption "Enables gwenview";
    config = mkOption {
      type = with types; nullOr (attrsOf (attrsOf (attrsOf (either bool (either int str)))));
      default = null;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.home.activation.gwenview = dagEntryAfter [ "linkGeneration" ] ''
      _() {
        ${builtins.concatStringsSep "\n" lines}
      } && _
      unset -f _
   '';

    apps.photoViewer = {
      name = "gwenview";
      command = "gwenview";
      desktopFile = "org.kde.gwenview.desktop";
      package = pkgs.libsForQt5.gwenview;
    };

    home-manager.users.cory.home.file = {
      ".local/share/gwenview/shortcuts/sxiv-emacs".text = builtins.readFile ./sxiv-emacs;
    };
  };
}
