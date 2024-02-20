{ config, lib, pkgs, ... }:

{
  # Window Manager
  windowManagers.cory.xmonad-pc.enable = true;

  # Editor
  programs.cory.emacs = {
    enable = true;
    popup = true;
    fonts = {
      # monospace.size = 100;
      monospace = {
        package = pkgs.librecode;
        name = "Librecode";
        size = 115;
      };
      variable.size = 115;
    };
  };

  # Terminal
  programs.cory.kitty.enable = true;

  # Browser
  programs.cory.firefox.enable = true;

  # File Manager
  programs.cory.caja.enable = true;

  # Photo Viewer
  programs.cory.sxiv.enable = true;

  # Video Player
  apps.videoPlayer = {
    name = "vlc";
    command = "vlc";
    desktopFile = "vlc.desktop";
    package = pkgs.vlc;
  };

  # PDF Viewer
  programs.cory.zathura.enable = true;

  # Music Player
  apps.musicPlayer = {
    name = "strawberry";
    command = "strawberry";
    desktopFile = "org.strawberrymusicplayer.strawberry.desktop";
    package = pkgs.strawberry;
  };

  # Notifications
  services.cory.dunst.enable = true;

  # Compositor
  services.cory.picom = {
    enable = true;
    roundBorders = false;
    cornerRadius = 0;
  };

  # Discord
  programs.cory.discord = {
    enable = true;
    # css = builtins.readFile ../../config/discocss/skeuocord.theme.css;
    css = "";
    package = pkgs.discord-gpu;
  };

  # Shell
  programs.cory.bat.enable = true;
  programs.cory.neofetch.enable = true;
  programs.cory.nushell.enable = true;

  # Gestures
  services.cory.touchegg = {
    enable = true;
    config = ../../config/touchegg/xmonad.conf;
  };

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
      serif = {
        package = liberation_ttf;
        name = "Liberation Serif";
        size = 12;
      };
      sansSerif = {
        package = liberation_ttf;
        name = "Liberation Sans";
        size = 12;
      };
      monospace = {
        package = julia-mono-nerdfont;
        name = "JuliaMono Nerd Font";
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
    cursor = {
      theme = "Vanilla-DMZ";
      size = 32;
      package = pkgs.vanilla-dmz;
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

  home-manager.users.cory.home.file.".config/conky" = {
    source = ../../config/conky/pc;
    recursive = true;
  };

  environment.systemPackages = with pkgs; [

  ];

  home-manager.users.cory.home.packages = with pkgs; [

  ];
}
