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
      "pointerColor" = "#d8dee9";
      "pointerColor2" = "#000507";

      "foreground"  = "#d8dee9";
      "background"  = "[80]#000507";
      #"background"  = "#000507";
      "cursorColor" = "#d8dee9";
      "color0"      = "#3b4252";
      "color8"      = "#373e4d";
      "color1"      = "#bf616a";
      "color9"      = "#94545d";
      "color2"      = "#a3be8c";
      "color10"     = "#809575";
      "color3"      = "#ebcb8b";
      "color11"     = "#b29e75";
      "color4"      = "#81a1c1";
      "color12"     = "#68809a";
      "color5"      = "#b48ead";
      "color13"     = "#8c738c";
      "color6"      = "#88c0d0";
      "color14"     = "#6d96a5";
      "color7"      = "#e5e9f0";
      "color15"     = "#aeb3bb";

      "font" = "xft:JetBrainsMono Nerd Font Mono:style=Regular:size=10:antialias=true";
      "boldFont" = "xft:JetBrainsMono Nerd Font Mono:style=Bold:size=10:antialias=true";
      "italicFont" = "xft:VictorMono Nerd Font Mono:style=Italic:size=10:antialias=true";
      "boldItalicFont" = "xft:VictorMono Nerd Font Mono:style=Bold Italic:size=10:antialias=true";

      #"internalBorder" = "72";
      "internalBorder" = "60";
      "externalBorder" = "0";
      "cursorBlink" = "true";
      "cursorUnderline" = "false";
      "urgentOnBell" = "true";
      "depth" = "32";
      "perl-ext-common" = "default,tabbedex,keyboard-select,url-select";
      #"perl-ext-common" = "default";
      #"perl-lib" = "${config.home.profileDirectory}/lib/urxvt/perl";

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
