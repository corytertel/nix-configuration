{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.rxvt-unicode}/bin/urxvtc";
    theme = ./light.rasi;
  };
}
