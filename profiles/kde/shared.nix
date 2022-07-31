{ config, lib, pkgs, ... }:

{
  windowManagers.cory.kde = {
    enable = true;
    # rightWindowDecor = true;
  };

  # Editor
  programs.cory.emacs.enable = true;

  # Terminal
  programs.cory.konsole.enable = true;

  # Browser
  programs.cory.firefox = {
    enable = true;
    changeColor = false;
  };

  # File Manager
  programs.cory.dolphin.enable = true;

  # Photo Viewer
  programs.cory.gwenview = {
    enable = true;
    config = import ../../config/gwenview/config.nix;
  };

  # PDF Viewer
  programs.cory.qpdfview.enable = true;

  # Launcher
  services.cory.rofi.enable = true;

  # Set other apps
  apps = {
    photoEditor = {
      name = "photogimp";
      command = "gimp";
      desktopFile = "gimp.desktop";
      package = pkgs.photogimp;
    };
    musicPlayer = {
      name = "audacious";
      command = "audacious";
      desktopFile = "audacious.desktop";
      package = pkgs.audacious;
    };
    videoPlayer = {
      name = "mpc-qt";
      command = "mpc-qt";
      desktopFile = "mpc-qt.desktop";
      package = pkgs.mpc-qt;
    };
    archiver = {
      name = "ark";
      command = "ark";
      desktopFile = "org.kde.ark.desktop";
      package = pkgs.libsForQt5.ark;
    };
  };

  # Keybinds
  services.cory.sxhkd = {
    enable = true;
    keybindings = import ../../config/sxhkd/kde.nix { inherit config pkgs; };
  };

  # Gestures
  services.cory.touchegg.enable = true;

  # Discord
  programs.cory.discord = {
    enable = true;
    css = builtins.readFile ../../config/discocss/skeuocord.theme.css;
  };

  # Shell
  programs.cory.bat.enable = true;
  programs.cory.neofetch.enable = true;
  programs.cory.zsh.enable = true;

  # Aesthetics
  theme = with pkgs; {
    name = "PlainLight";
    darkTheme = false;
    gtk.enable = false;
    icons = {
      name = "crystal-nova";
      package = crystal-nova-icon-theme;
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
      color7          = "#ffffff";
      color15         = "#ffffff";
    };
  };

  environment.systemPackages = with pkgs; [
    crystal-remix-icon-theme
    oxygen-kde4-theme
    libsForQt5.oxygen
    oxygen-cory-colors
    expose-glassy
    # expose-glassy-right
    nova7-icon-theme
    crystal-nova-icon-theme
  ];

  home-manager.users.cory.home.packages = with pkgs; [

  ];
}
