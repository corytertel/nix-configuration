{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.xfce.xfce4-terminal}/bin/xfce4-terminal";
    theme = ./light-mountain.rasi;
  };
}
