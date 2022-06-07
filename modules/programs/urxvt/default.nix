{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.urxvt;
in {
  options.programs.cory.urxvt = {
    enable = mkEnableOption "Enables urxvt";
    iconFile = mkOption {
      type = types.str;
      default = "${config.theme.icons.package}/share/icons/${config.theme.icons.name}/48x48/apps/utilities-terminal.png";
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.programs.urxvt = {
      enable = true;
      package = pkgs.rxvt-unicode-emoji;
      extraConfig = with config.theme; {
        "termName" = "rxvt";
        "visualBell" = "false";
        "loginShell" = "true";
        "geometry" = "120x33";

        "colorMode" = "true";
        "pointerColor" = color.background;
        "pointerColor2" = color.foreground;

        "foreground"  = color.foreground;
        "background"  = color.background;
        "cursorColor" = color.foreground;
        "color0"      = color.color0;
        "color8"      = color.color8;
        "color1"      = color.color1;
        "color9"      = color.color9;
        "color2"      = color.color2;
        "color10"     = color.color10;
        "color3"      = color.color3;
        "color11"     = color.color11;
        "color4"      = color.color4;
        "color12"     = color.color12;
        "color5"      = color.color5;
        "color13"     = color.color13;
        "color6"      = color.color6;
        "color14"     = color.color14;
        "color7"      = color.color7;
        "color15"     = color.color15;

        "font" = "xft:${font.monospace.name}:style=Regular:size=${toString (font.monospace.size)}:antialias=true";
        "boldFont" = "xft:${font.monospace.name}:style=Bold:size=${toString (font.monospace.size)}:antialias=true";
        "italicFont" = "xft:${font.monospace.name}:style=Italic:size=${toString (font.monospace.size)}:antialias=true";
        "boldItalicFont" = "xft:${font.monospace.name}:style=Bold Italic:size=${toString (font.monospace.size)}:antialias=true";

        "internalBorder" = "40";
        "externalBorder" = "0";
        "cursorBlink" = "true";
        "cursorUnderline" = "false";
        "urgentOnBell" = "true";
        "depth" = "32";
        "perl-ext-common" = "default";

        "iconFile" = cfg.iconFile;

        "urlLauncher" = "firefox";
        "underlineURLs" = "true";
        "urlButton" = "1";
      };
      iso14755 = false;
      keybindings = {
        "Shift-Control-V" = "eval:paste_clipboard";
        "Shift-Control-C" = "eval:selection_to_clipboard";
      };
      scroll = {
        bar.enable = false;
        keepPosition = true;
        lines = 5000;
        scrollOnKeystroke = true;
        scrollOnOutput = false;
      };
    };
  };
}
