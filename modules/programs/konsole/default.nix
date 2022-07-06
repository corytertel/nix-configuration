{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.konsole;

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

  configs = import ../../../config/konsole/config.nix { inherit config; };

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
  options.programs.cory.konsole = {
    enable = mkEnableOption "Enables konsole";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.home.activation.konsole = dagEntryAfter [ "linkGeneration" ] ''
      _() {
        ${builtins.concatStringsSep "\n" lines}
      } && _
      unset -f _
   '';

    apps.terminal = {
      name = "konsole";
      command = "konsole";
      desktopFile = "org.kde.konsole.desktop";
      package = pkgs.libsForQt5.konsole;
    };
  };
}
