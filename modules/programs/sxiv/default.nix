{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.sxiv;
in {
  options.programs.cory.sxiv = {
    enable = mkEnableOption "Enables sxiv";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory = {
      home.packages = [ pkgs.sxiv ];

      xresources.extraConfig = ''
        Sxiv.background: ${config.theme.color.background}
        Sxiv.foreground: ${config.theme.color.foreground}
        Sxiv.font: ${config.theme.font.system.name}:size=${toString config.theme.font.system.size}
      '';
    };
  };
}
