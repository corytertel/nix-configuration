{ config, lib, pkgs, ... }:

{
  imports = [
    ./pc.nix
  ];

  # Window Manager
  windowManagers.cory.fvwm.pc.enable = true;

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

  # Video Player
  programs.cory.mpc-qt.enable = true;

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
    archiver = {
      name = "ark";
      command = "ark";
      desktopFile = "org.kde.ark.desktop";
      package = pkgs.libsForQt5.ark;
    };
    launcher = {
      name = "emacs-run-launcher";
      command = "emacsclient -e '(emacs-run-launcher)'";
      desktopFile = "emacsclient.desktop";
      package = pkgs.emacsNativeComp;
    };
  };

  # Notifications
  services.cory.dunst.enable = true;

  # Compositor
  services.cory.picom = {
    enable = true;
    roundBorders = true;
    cornerRadius = 5;
  };

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
    gtk = {
      enable = true;
      name = "Breeze";
      package = pkgs.libsForQt5.breeze-gtk;
    };
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

  home-manager.users.cory.home.file.".config/gtk-3.0" = {
    source = ./gtk-3.0;
    recursive = true;
  };

  home-manager.users.cory.home.file.".config/qt5ct" = {
    source = ./qt5ct;
    recursive = true;
  };

  environment.systemPackages = with pkgs; [

  ];

  home-manager.users.cory.home.packages = with pkgs; [

  ];
}
