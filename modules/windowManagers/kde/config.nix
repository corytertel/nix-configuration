# Credit to LunNova for supplying the original file
# I would have never been able to figure out KDE file patching

{ config, lib, pkgs, ... }:

let
  dagEntryAfter = after: data: {
    inherit data after;
    before = [ ];
  };

  toValue = v:
    if builtins.isString v then
      v
    else if builtins.isBool v then
      lib.boolToString v
    else if builtins.isInt v then
      builtins.toString v
    else
      builtins.abort ("Unknown value type: " ++ builtins.toString v);

  configs = import ../../../config/kde/config.nix { inherit config; };

  lines = lib.flatten (lib.mapAttrsToList
    (file:
      lib.mapAttrsToList
        (group:
          lib.mapAttrsToList
            (key: value:
              "$DRY_RUN_CMD ${pkgs.libsForQt5.kconfig}/bin/kwriteconfig5 --file $confdir/'${file}' --group '${group}' --key '${key}' '${
                toValue value
              }'")
        ))
    configs);

in {
  home.activation.kwriteconfig5 = dagEntryAfter [ "linkGeneration" ] ''
    _() {
      confdir="''${XDG_CONFIG_HOME:-$HOME/.config}"
      ${builtins.concatStringsSep "\n" lines}
      $DRY_RUN_CMD ${pkgs.libsForQt5.qt5.qttools.bin}/bin/qdbus org.kde.KWin /KWin reconfigure || echo "KWin reconfigure failed"
      for i in {0..10}; do
        $DRY_RUN_CMD ${pkgs.dbus}/bin/dbus-send --type=signal /KGlobalSettings org.kde.KGlobalSettings.notifyChange int32:$i int32:0 || echo "KGlobalSettings.notifyChange failed"
      done
    } && _
    unset -f _
  '';

  home.activation.extraGtkConfig = let
    insertLine = line: file: ''
      $DRY_RUN_CMD grep -qxF ${line} ${file} || echo ${line} >> ${file}
    '';
  in dagEntryAfter ["linkGeneration"] ''
    ${insertLine "\'gtk-key-theme-name=\"Emacs\"\'" "$HOME/.gtkrc-2.0"}
    ${insertLine "\'gtk-key-theme-name = \"Emacs\"\'" "$HOME/.config/gtkrc-2.0"}
    ${insertLine "\'gtk-key-theme-name=Emacs\'" "$HOME/.config/gtk-3.0/settings.ini"}
    ${insertLine "\'gtk-key-theme-name=Emacs\'" "$HOME/.config/gtk-4.0/settings.ini"}
  '';

  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/interface" = with config.theme; {
        icon-theme = "${icons.name}";
        document-font-name = "${font.system.name} ${toString font.system.size}";
        monospace-font-name = "${font.monospace.name} ${toString font.monospace.size}";
        gtk-key-theme = "Emacs";
      };
    };
  };
}
