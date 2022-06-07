{ config, lib, pkgs, ... }:

{
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
  programs.cory.sxiv.enable = true;
  programs.cory.ungoogled-chromium.enable = true;
  programs.cory.urxvt = with config.theme; {
    enable = true;
    iconFile = "${icons.package}/share/icons/${icons.name}/apps/64/utilities-terminal.svg";
  };
  programs.cory.zathura.enable = true;
  programs.cory.zsh.enable = true;

  theme = with pkgs; {
    name = "PlainLight";
    darkTheme = false;
    icons = {
      # package = numix-icon-theme-square;
      # name = "Numix-Square-Light";
      package = newaita-reborn-icon-theme;
      name = "Newaita-reborn";
      size = 54;
    };
    font = {
      system = {
        package = noto-nerdfont;
        name = "NotoSans Nerd Font";
        size = 10;
      };
      monospace = {
        package = victor-mono-nerdfont;
        name = "VictorMono Nerd Font";
        size = 10;
      };
    };
    color = {
      foreground      = "#141404";
      background      = "#ffffff";
      background-alt1 = "#eeeeee";
      background-alt2 = "#e8e8e8";
      background-alt3 = "#dddddd";
      background-alt4 = "#cccccc";
      color0          = "#141404";
      color8          = "#141404";
      color1          = "#e60909";
      color9          = "#e60909";
      color2          = "#1f8c35";
      color10         = "#1f8c35";
      color3          = "#ed8f23";
      color11         = "#ed8f23";
      color4          = "#3647d9";
      color12         = "#3647d9";
      color5          = "#e01bd0";
      color13         = "#e01bd0";
      color6          = "#2d9574";
      color14         = "#2d9574";
      color7          = "#cccccc";
      color15         = "#cccccc";
    };
  };
}
