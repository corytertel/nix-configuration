{ pkgs, config, ... }:

{
  programs.urxvt = {
    enable = true;
    extraConfig = {
      "termName" = "rxvt";
      "visualBell" = "false";
      "loginShell" = "true";

      "colorMode" = "true";
      "pointerColor" = "#f0f0f0";
      "pointerColor2" = "#0f0f0f";

      "foreground"  = "#3c3836";
      #"background"  = "[98]#3c3836";
      "background"  = "#fbf1c7";
      "cursorColor" = "#3c3836";
      "color0"      = "#7c6f64";
      "color8"      = "#928374";
      "color1"      = "#cc241d";
      "color9"      = "#9d0006";
      "color2"      = "#98971a";
      "color10"     = "#79740e";
      "color3"      = "#d79921";
      "color11"     = "#b57614";
      "color4"      = "#458588";
      "color12"     = "#076678";
      "color5"      = "#b16286";
      "color13"     = "#8f3f71";
      "color6"      = "#689d6a";
      "color14"     = "#427b58";
      "color7"      = "#d65d0e";
      "color15"     = "#af3a03";

      "internalBorder" = "10";
      "cursorBlink" = "true";
      "cursorUnderline" = "false";
      "urgentOnBell" = "true";
      "depth" = "32";
      #"perl-ext-common" = "default,tabbedex,keyboard-select,url-select";
      "perl-ext-common" = "default";
      #"perl-lib" = "${config.home.profileDirectory}/lib/urxvt/perl";

      "urlLauncher" = "firefox";
      "underlineURLs" = "true";
      "urlButton" = "1";
    };
    fonts = [
      "xft:JetBrainsMono Nerd Font Mono:style=Regular:size=10:antialias=true"
    ];
    iso14755 = false;
    keybindings = {
      #"Shift-Up" = "command:\033]720;1\007";
      #"Shift-Down" = "command:\033]721;1\007";
      #"Control-Up" = "\033[1;5A";
      #"Control-Down" = "\033[1;5B";
      #"Control-Right" = "\033[1;5C";
      #"Control-Left" = "\033[1;5D";
      "Shift-Control-V" = "eval:paste_clipboard";
      "Shift-Control-C" = "eval:selection_to_clipboard";

      #"Control-Escape" = "perl:keyboard-select:activate";
      #"Control-S" = "perl:keyboard-select:search";
      #"Control-U" = "perl:url-select:select_next";
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
