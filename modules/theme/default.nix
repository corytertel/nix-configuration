{ config, lib, pkgs, ... }:
with lib;

let

  mkColorOption = d: mkOption {
    type = types.str;
    default = d;
  };

in {
  options.theme = {

    name = mkOption {
      type = types.str;
      default = "MyTheme";
    };

    darkTheme = mkOption {
      type = types.bool;
      default = false;
    };

    gtk = {

      enable = mkEnableOption "Enables a gtk theme";

      name = mkOption {
        type = types.nullOr types.str;
        default = null;
      };

      package = mkOption {
        type = types.nullOr types.package;
        default = null;
      };

    };

    icons = {

      package = mkOption {
        type = types.package;
        default = pkgs.hicolor-icon-theme;
      };

      name = mkOption {
        type = types.str;
        default = "";
      };

      size = mkOption {
        type = types.int;
        default = 48;
      };

    };

    font = {

      system = {

        package = mkOption {
          type = types.nullOr types.package;
          default = null;
          description = "The system-wide font. Will be used everywhere except where a monospace font is forced.";
        };

        name = mkOption {
          type = types.str;
          default = "";
        };

        size = mkOption {
          type = types.int;
          default = 10;
        };

      };

      monospace = {

        package = mkOption {
          type = types.nullOr types.package;
          default = null;
          description = "The font used when a monospace font is forced.";
        };

        name = mkOption {
          type = types.str;
          default = "";
        };

        size = mkOption {
          type = types.int;
          default = 10;
        };

      };

    };

    color = {
      background = mkColorOption "#000000";
      background-alt1 = mkColorOption "#000000";
      background-alt2 = mkColorOption "#000000";
      background-alt3 = mkColorOption "#000000";
      background-alt4 = mkColorOption "#000000";
      foreground = mkColorOption "#000000";
      color0 = mkColorOption "#000000";
      color1 = mkColorOption "#000000";
      color2 = mkColorOption "#000000";
      color3 = mkColorOption "#000000";
      color4 = mkColorOption "#000000";
      color5 = mkColorOption "#000000";
      color6 = mkColorOption "#000000";
      color7 = mkColorOption "#000000";
      color8 = mkColorOption "#000000";
      color9 = mkColorOption "#000000";
      color10 = mkColorOption "#000000";
      color11 = mkColorOption "#000000";
      color12 = mkColorOption "#000000";
      color13 = mkColorOption "#000000";
      color14 = mkColorOption "#000000";
      color15 = mkColorOption "#000000";
    };

  };

  config = mkIf config.theme.gtk.enable {
    programs.cory.gtk.enable = true;
  };

}
