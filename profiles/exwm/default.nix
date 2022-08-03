{ config, lib, pkgs, ... }:

{
  # Window Manager and Editor
  windowManagers.cory.exwm.enable = true;

  # Browser
  programs.cory.firefox.enable = true;

  # PDF Viewer
  programs.cory.qpdfview.enable = true;

  # Set other apps
  apps = {
    terminal = {
      name = "emacs";
      command = "emacsclient -e '(eshell)'";
      desktopFile = "emacsclient.desktop";
      package = pkgs.emacsNativeComp;
    };
    photoViewer = {
      name = "emacs";
      command = "emacsclient";
      desktopFile = "emacsclient.desktop";
      package = pkgs.emacsNativeComp;
    };
    photoEditor = {
      # No need to compile photogimp here
      name = "gimp";
      command = "gimp";
      desktopFile = "gimp.desktop";
      package = pkgs.gimp;
    };
    musicPlayer = {
      name = "audacious";
      command = "audacious";
      desktopFile = "audacious.desktop";
      package = pkgs.audacious;
    };
    videoPlayer = {
      name = "mpv";
      command = "mpv";
      desktopFile = "mpv.desktop";
      package = pkgs.mpv;
    };
    fileManager = {
      name = "emacs";
      command = "emacsclient -e '(dired \"/home/cory\")'";
      desktopFile = "emacsclient.desktop";
      package = pkgs.emacsNativeComp;
    };
    archiver = {
      name = "lxqt-archiver";
      command = "lxqt-archiver";
      desktopFile = "lxqt-archiver.desktop";
      package = pkgs.lxqt.lxqt-archiver;
    };
    launcher = {
      name = "emacs";
      command = "emacsclient -e '(async-shell-command)'";
      desktopFile = "emacsclient.desktop";
      package = pkgs.emacsNativeComp;
    };
  };

  # Notifications
  services.cory.dunst.enable = true;

  # Compositor
  services.cory.picom.enable = true;

  # Shell
  programs.cory.neofetch.enable = true;

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

  ];

  home-manager.users.cory.home.packages = with pkgs; [

  ];
}
