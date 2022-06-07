{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.cory.sxhkd;

  keybindingsStr = concatStringsSep "\n" (mapAttrsToList (hotkey: command:
    optionalString (command != null) ''
      ${hotkey}
        ${command}
    '') cfg.keybindings);
in {
  options.services.cory.sxhkd = {
    enable = mkEnableOption "Enables sxhkd";
    keybindings = mkOption {
      type = types.attrsOf (types.nullOr types.str);
      default = { };
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.cory = {

      xdg.configFile."sxhkd/sxhkdrc".text =
        concatStringsSep "\n" [ keybindingsStr "" ];

      home.packages = [ pkgs.sxhkd ];

    };
  };
}
