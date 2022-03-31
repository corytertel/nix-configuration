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
      "intensityStyles" = "false";
      "pointerColor" = "#ffffff";
      "pointerColor2" = "000000";

      "foreground" = "#000000";
      "background" = "#ffffea";
      "color0"     = "#000000";
      "color8"     = "#eaeaea";
      "color1"     = "#880000";
      "color9"     = "#ffeaea";
      "color2"     = "#005500";
      "color10"    = "#eaffea";
      "color3"     = "#663311";
      "color11"    = "#eeee9e";
      "color4"     = "#004488";
      "color12"    = "#cceeff";
      "color5"     = "#770077";
      "color13"    = "#ffeaff";
      "color6"     = "#007777";
      "color14"    = "#eaffff";
      "color7"     = "#eeeecc";
      "color15"    = "#ffffea";

      "font" = "xft:VictorMono Nerd Font Mono:style=Regular:size=10:antialias=true";
      "boldFont" = "xft:VictorMono Nerd Font Mono:style=Bold:size=10:antialias=true";
      "italicFont" = "xft:VictorMono Nerd Font Mono:style=Italic:size=10:antialias=true";
      "boldItalicFont" = "xft:VictorMono Nerd Font Mono:style=Bold Italic:size=10:antialias=true";

      "internalBorder" = "30";
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
