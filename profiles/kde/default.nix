{ config, lib, pkgs, ... }:

{
  windowManagers.cory.kde.enable = true;

  # services.cory.plank.enable = true;
  services.cory.rofi.enable = true;
  services.cory.sxhkd = {
    enable = true;
    keybindings = import ../../config/sxhkd/kde.nix;
  };
  # services.cory.tint2.enable = true;
  services.cory.touchegg.enable = true;

  programs.cory.bash.enable = true;
  programs.cory.discord = {
    enable = true;
    css = builtins.readFile ../../config/discocss/skeuocord.theme.css;
  };
  programs.cory.firefox = {
    enable = true;
    changeColor = false;
  };
  programs.cory.layout_switch.enable = true;
  programs.cory.neofetch.enable = true;
  programs.cory.sxiv.enable = true;
  programs.cory.ungoogled-chromium.enable = true;
  programs.cory.urxvt.enable = true;
  programs.cory.zathura.enable = true;
  programs.cory.zsh.enable = true;

  theme = with pkgs; {
    name = "PlainLight";
    darkTheme = false;
    gtk.enable = false;
    icons = {
      name = "crystal-remix";
      package = pkgs.crystal-remix-icon-theme;
    };
    font = {
      system = {
        package = oxygen-nerdfont;
        name = "Oxygen Nerd Font";
        size = 11;
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

  environment.systemPackages = with pkgs; [
    crystal-remix-icon-theme
    oxygen-kde4-theme
    libsForQt5.oxygen
    nomanssky-theme
    libsForQt5.qt5.qttools
    libsForQt5.kde-gtk-config
  ];

  home-manager.users.cory.home.packages = with pkgs; [

  ];
}