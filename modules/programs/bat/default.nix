{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.bat;

  dagEntryAfter = after: data: {
    inherit data after;
    before = [ ];
  };


  toConfigFile = generators.toKeyValue {
    mkKeyValue = k: v: "--${k}=${lib.escapeShellArg v}";
    listsAsDuplicateKeys = true;
  };

  bat-config = {
    pager = "less -R";
    theme = config.theme.name;
    style = "plain";
  };

  bat-themes = {
    "${config.theme.name}" = import ./theme.nix { inherit config; };
  };

in {
  options.programs.cory.bat = {
    enable = mkEnableOption "bat, a cat clone with wings";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory = {

      # from home-manager bat config
      xdg.configFile = mkMerge ([{
        "bat/config" =
          mkIf (bat-config != { }) { text = toConfigFile bat-config; };
      }] ++ flip mapAttrsToList bat-themes
        (name: body: { "bat/themes/${name}.tmTheme" = { text = body; }; }));

      # all this work bc i don't want to add bat to my path
      programs.zsh.shellAliases = {
        cat = "${pkgs.bat}/bin/bat --paging=never";
        less = "${pkgs.bat}/bin/bat --paging=always";
      };

      programs.zsh.sessionVariables = {
        MANPAGER = "${pkgs.bashInteractive}/bin/sh -c 'col -bx | ${pkgs.bat}/bin/bat -l man -p'";
      };

      home.activation.batcache = dagEntryAfter [ "linkGeneration" ] ''
        $DRY_RUN_CMD ${pkgs.bat}/bin/bat cache --build
      '';
    };
  };
}
