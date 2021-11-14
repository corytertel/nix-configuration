{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.kitty}/bin/kitty";
    theme = ./mountain2.rasi;
  };
}
