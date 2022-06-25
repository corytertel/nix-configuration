{ config, lib, pkgs, ... }:
with lib;

let

  cfg = config.programs.cory.bat;

  toConfigFile = generators.toKeyValue {
    mkKeyValue = k: v: "--${k}=${lib.escapeShellArg v}";
    listsAsDuplicateKeys = true;
  };

  bat-config = {
    pager = "less -R";
    theme = "PlainLight";
    style = "plain";
  };

  bat-themes = {
    PlainLight = builtins.readFile ../../../config/bat/PlainLight.tmTheme;
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

    };
  };
}
