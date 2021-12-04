{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.kitty}/bin/kitty";
    theme = ./Arc.rasi;
  };
}
