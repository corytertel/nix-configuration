{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.krusader;

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
              "$DRY_RUN_CMD ${pkgs.libsForQt5.kconfig}/bin/kwriteconfig5 --file $confdir/'${file}' --group '${group}' --key '${key}' '${
                toValue value
              }'")
        ))
    cfg.config);

in {
  options.programs.cory.krusader = {
    enable = mkEnableOption "Enable krusader";
    config = mkOption {
      type = with types; nullOr (attrsOf (attrsOf (attrsOf (either bool (either int str)))));
      default = null;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.home.packages = with pkgs; [
      krusader
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
    ];

    home-manager.users.cory.home.activation.krusader = dagEntryAfter [ "linkGeneration" ] ''
      _() {
        confdir="''${XDG_CONFIG_HOME:-$HOME/.config}"
        ${builtins.concatStringsSep "\n" lines}
      } && _
      unset -f _
   '';

    # apps.fileManager = {
    #   name = "krusader";
    #   command = "krusader";
    #   desktopFile = "org.kde.krusader.desktop";
    #   package = pkgs.krusader;
    # };
  };
}
