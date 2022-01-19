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
      "pointerColor2" = "#c49ea0";

      "foreground"  = "#0f0f0f";
      #"background"  = "[98]#f0f0f0";
      "background"  = "#f0f0f0";
      "cursorColor" = "#0f0f0f";
      "color0"      = "#b2b2b2";
      "color8"      = "#d9d9d9";
      "color1"      = "#541015";
      "color9"      = "#3a0b0d";
      "color2"      = "#105413";
      "color10"     = "#0b3a0c";
      "color3"      = "#544e10";
      "color11"     = "#3a360b";
      "color4"      = "#1a1054";
      "color12"     = "#110b3a";
      "color5"      = "#541054";
      "color13"     = "#3a0b3a";
      "color6"      = "#105451";
      "color14"     = "#0b393a";
      "color7"      = "#262626";
      "color15"     = "#4c4c4c";

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
      "xft:TerminessTTF Nerd Font Mono:style=Regular:size=11:antialias=true"
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
