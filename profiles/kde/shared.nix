{ config, lib, pkgs, ... }:

{
  windowManagers.cory.kde.enable = true;

  # Terminal
  # programs.cory.urxvt.enable = true;
  programs.cory.konsole.enable = true;

  # Browser
  programs.cory.firefox = {
    enable = true;
    changeColor = false;
    windowColor = "#e0dfde";
    secondaryColor = "#c8c6c4";
    invertedColor = "#222120";
  };

  # Photo Viewer
  programs.cory.lximage-qt.enable = true;

  # PDF Viewer
  programs.cory.qpdfview.enable = true;

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
      name = "vlc";
      command = "vlc";
      desktopFile = "vlc.desktop";
      package = pkgs.vlc;
    };
    fileManager = {
      name = "pcmanfm-qt";
      command = "pcmanfm-qt --new-window";
      desktopFile = "pcmanfm-qt.desktop";
      package = pkgs.pcmanfm-qt;
    };
    archiver = {
      name = "lxqt-archiver";
      command = "lxqt-archiver";
      desktopFile = "lxqt-archiver.desktop";
      package = pkgs.lxqt.lxqt-archiver;
    };
    launcher = {
      name = "krunner";
      command = "krunner";
      desktopFile = "krunner.desktop";
      package = pkgs.libsForQt5.plasma-workspace;
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
      name = "crystal-remix";
      package = crystal-remix-icon-theme;
      # name = "nova7";
      # package = nova7-icon-theme;
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
      # color7          = "#cccccc";
      # color15         = "#cccccc";
      color7          = "#ffffff";
      color15         = "#ffffff";
    };
  };

  environment.systemPackages = with pkgs; [
    crystal-remix-icon-theme
    oxygen-kde4-theme
    libsForQt5.oxygen
    expose-glassy

    nova7-icon-theme
  ];

  home-manager.users.cory.home.packages = with pkgs; [

  ];
}
