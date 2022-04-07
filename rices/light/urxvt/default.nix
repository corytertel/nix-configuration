{ pkgs, config, ... }:

{
  programs.urxvt = {
    enable = true;
    extraConfig = {
      "termName" = "rxvt";
      "visualBell" = "false";
      "loginShell" = "true";
      "geometry" = "85x33";

      "colorMode" = "true";
      "pointerColor" = "#ffffff";
      "pointerColor2" = "#141404";

      "foreground"  = "#141404";
      "background"  = "#ffffff";
      "cursorColor" = "#141404";
      "color0"      = "#141404";
      "color8"      = "#141404";
      "color1"      = "#e60909";
      "color9"      = "#e60909";
      "color2"      = "#1f8c35";
      "color10"     = "#1f8c35";
      "color3"      = "#ed8f23";
      "color11"     = "#ed8f23";
      "color4"      = "#3647d9";
      "color12"     = "#3647d9";
      "color5"      = "#e01bd0";
      "color13"     = "#e01bd0";
      "color6"      = "#2d9574";
      "color14"     = "#2d9574";
      "color7"      = "#cccccc";
      "color15"     = "#cccccc";

      "font" = "xft:VictorMono Nerd Font Mono:style=Regular:size=10:antialias=true";
      "boldFont" = "xft:VictorMono Nerd Font Mono:style=Bold:size=10:antialias=true";
      "italicFont" = "xft:VictorMono Nerd Font Mono:style=Italic:size=10:antialias=true";
      "boldItalicFont" = "xft:VictorMono Nerd Font Mono:style=Bold Italic:size=10:antialias=true";

      "internalBorder" = "40";
      "externalBorder" = "0";
      "cursorBlink" = "true";
      "cursorUnderline" = "false";
      "urgentOnBell" = "true";
      "depth" = "32";
      "perl-ext-common" = "default";

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
}
