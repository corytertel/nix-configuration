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

      "foreground"  = "#f0f0f0";
      "background"  = "[98]#0f0f0f";
      "cursorColor" = "#f0f0f0";
      "color0"      = "#262626";
      "color8"      = "#4c4c4c";
      "color1"      = "#ac8a8c";
      "color9"      = "#c49ea0";
      "color2"      = "#8aac8b";
      "color10"     = "#9ec49f";
      "color3"      = "#aca98a";
      "color11"     = "#c4c19e";
      "color4"      = "#8f8aac";
      "color12"     = "#a39ec4";
      "color5"      = "#ac8aac";
      "color13"     = "#c49ec4";
      "color6"      = "#8aabac";
      "color14"     = "#9ec3c4";
      "color7"      = "#e7e7e7";
      "color15"     = "#f5f5f5";

      "internalBorder" = "48";
      "cursorBlink" = "true";
      "cursorUnderline" = "false";
      "urgentOnBell" = "true";
      "depth" = "32";
      "perl-ext-common" = "default,tabbedex,keyboard-select,url-select";
      #"perl-lib" = "${config.home.profileDirectory}/lib/urxvt/perl";

      "urlLauncher" = "firefox";
      "underlineURLs" = "true";
      "urlButton" = "1";
    };
    fonts = [
      "xft:FantasqueSansMono Nerd Font:style=Regular:size=12"
      "xft:FantasqueSansMono Nerd Font:style=Bold:size=12"
      "xft:FantasqueSansMono Nerd Font:style=Italic:size=12"
      "xft:FantasqueSansMono Nerd Font:style=Bold Italic:size=12"
      "xft:Font Awesome 5 Free Regular:style=Regular:size11"
      "xft:Font Awesome 5 Free Solid:style=Solid:size11"
      "xft:Font Awesome 5 Brands Regular:style=Regular:size11"
    ];
    iso14755 = true;
    keybindings = {
      "Shift-Up" = "command:\033]720;1\007";
      "Shift-Down" = "command:\033]721;1\007";
      "Control-Up" = "\033[1;5A";
      "Control-Down" = "\033[1;5B";
      "Control-Right" = "\033[1;5C";
      "Control-Left" = "\033[1;5D";
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
