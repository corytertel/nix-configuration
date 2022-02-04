{ pkgs, config, ... }:

{
  programs.urxvt = {
    enable = true;
    extraConfig = {
      #"termName" = "rxvt-256color";
      "termName" = "rxvt";
      "visualBell" = "false";
      "loginShell" = "true";
      "geometry" = "85x33";

      "colorMode" = "true";
      "pointerColor" = "#30343f";
      "pointerColor2" = "#fefefe";

      "foreground"  = "#30343f";
      "background"  = "#fefefe";
      #"background"  = "[80]#fefefe";
      "cursorColor" = "#30343f";
      "color0"      = "#636d77";
      "color8"      = "#737a8a";
      "color1"      = "#e96179";
      "color9"      = "#ef6a7b";
      "color2"      = "#99cd61";
      "color10"     = "#a1d569";
      "color3"      = "#ef8b2b";
      "color11"     = "#f69335";
      "color4"      = "#49bae6";
      "color12"     = "#4ec2e9";
      "color5"      = "#f6bfc5";
      "color13"     = "#fec7cd";
      "color6"      = "#8db9b8";
      "color14"     = "#95c1c0";
      "color7"      = "#d5dae0";
      "color15"     = "#dde2e8";

      "font" = "xft:JetBrainsMono Nerd Font Mono:style=Regular:size=10:antialias=true";
      "boldFont" = "xft:JetBrainsMono Nerd Font Mono:style=Bold:size=10:antialias=true";
      "italicFont" = "xft:VictorMono Nerd Font Mono:style=Italic:size=10:antialias=true";
      "boldItalicFont" = "xft:VictorMono Nerd Font Mono:style=Bold Italic:size=10:antialias=true";

      "internalBorder" = "45";
      "externalBorder" = "0";
      "cursorBlink" = "true";
      "cursorUnderline" = "false";
      "urgentOnBell" = "true";
      "depth" = "32";
      "perl-ext-common" = "default,tabbedex,keyboard-select,url-select";

      "urlLauncher" = "firefox";
      "underlineURLs" = "true";
      "urlButton" = "1";
    };
    #fonts = [
    #  "xft:JetBrainsMono Nerd Font Mono:style=Regular:size=10:antialias=true"
    #];
    iso14755 = false;
    keybindings = {
      "Shift-Up" = "command:\033]720;1\007";
      "Shift-Down" = "command:\033]721;1\007";
      "Control-Up" = "command:\033[1;5A";
      "Control-Down" = "command:\033[1;5B";
      "Control-Right" = "command:\033[1;5C";
      "Control-Left" = "command:\033[1;5D";

      "Shift-Control-V" = "eval:paste_clipboard";
      "Shift-Control-C" = "eval:selection_to_clipboard";

      "Control-Escape" = "perl:keyboard-select:activate";
      "Control-S" = "perl:keyboard-select:search";
      "Control-U" = "perl:url-select:select_next";
    };
    scroll = {
      bar.enable = false;
      keepPosition = true;
      lines = 5000;
      scrollOnKeystroke = true;
      scrollOnOutput = false;
    };
  };

  home.file = {
    ".urxvt/ext/tabbedex".text = builtins.readFile ./tabbedex;
    ".urxvt/ext/keyboard-select".text = builtins.readFile ./keyboard-select;
    ".urxvt/ext/url-select".text = builtins.readFile ./url-select;
    ".urxvt/ext/clipboard".text = builtins.readFile ./clipboard;
  };
}
