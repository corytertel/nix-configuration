{ pkgs, config, ... }:

{
  home.file.".icons/48/apps/utilities-terminal.png".source = ./utilities-terminal.png;
  programs.urxvt = {
    enable = true;
    extraConfig = {
      "termName" = "rxvt";
      "visualBell" = "false";
      "loginShell" = "true";
      "geometry" = "120x33";

      "colorMode" = "true";
      "pointerColor" = "#ffffff";
      "pointerColor2" = "#141404";

      "foreground"  = "#ffffff";
      "background"  = "#141404";
      "cursorColor" = "#ffffff";
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

      "internalBorder" = "40";
      "externalBorder" = "0";
      "cursorBlink" = "true";
      "cursorUnderline" = "false";
      "urgentOnBell" = "true";
      "depth" = "32";
      "perl-ext-common" = "default";

      "iconFile" = "/home/cory/.icons/48/apps/utilities-terminal.png";

      "urlLauncher" = "firefox";
      "underlineURLs" = "true";
      "urlButton" = "1";
    };
    fonts = [ "xft:CaskaydiaCove Nerd Font Mono:size=10:antialias=true" ];
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
