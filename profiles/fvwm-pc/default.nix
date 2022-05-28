{ config, lib, pkgs, ... }:

let
  theme = {
    icons = rec {
      package = pkgs.hello;
      name = "";
      path = "${package}/share/icons/${name}";
    };
    fonts = {
      system = {
        name = "";
        size = 10;
      };
      monospace = {
        name = "";
        size = 10;
      };
    };
    colors = {
      background = "#";
      background-alt1 = "#";
      background-alt2 = "#";
      background-alt3 = "#";
      background-alt4 = "#";
      foreground = "#";
      color0 = "#";
      color1 = "#";
      color2 = "#";
      color3 = "#";
      color4 = "#";
      color5 = "#";
      color6 = "#";
      color7 = "#";
      color8 = "#";
      color9 = "#";
      color10 = "#";
      color11 = "#";
      color12 = "#";
      color13 = "#";
      color14 = "#";
      color15 = "#";
    };
  };
in {
  windowManagers.cory.fvwm.pc.enable = true;

  services.cory.dunst.enable = true;
  services.cory.picom.enable = true;
  services.cory.plank.enable = true;
  services.cory.rofi.enable = true;
  services.cory.tint2.enable = true;

  programs.cory.bash.enable = true;
  programs.cory.discord.enable = true;
  programs.cory.firefox.enable = true;
  programs.cory.gtk.enable = true;
  programs.cory.layout_switch.enable = true;
  programs.cory.neofetch.enable = true;
  programs.cory.ungoogled-chromium.enable = true;
  programs.cory.urxvt.enable = true;
  programs.cory.zathura.enable = true;
  programs.cory.zsh.enable = true;
}
