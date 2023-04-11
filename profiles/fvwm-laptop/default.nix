{ config, lib, pkgs, ... }:

{
  # Window Manager
  windowManagers.cory.fvwm.laptop.enable = true;

  # Editor
  programs.cory.emacs.enable = true;

  # Terminal
  # programs.cory.konsole.enable = true;
  home-manager.users.cory.programs.kitty = {
    enable = true;
    font = {
      name = "JuliaMono";
      size = 10;
    };
    settings = with config.theme.color; {
      disable_ligatures = "cursor";
      cursor_blink_interval = "0.5";
      cursor_stop_blinking_after = 0;
      cursor_shape = "beam";
      scrollback_lines = 5000;
      enable_audio_bell = "yes";
      update_check_interval = 0;
      repaint_delay = 10;
      input_delay = 3;
      sync_to_monitor = "yes";
      remember_window_size = "no";
      initial_window_width = 1280;
      initial_window_height = 800;
      window_padding_width = 10;
      confirm_os_window_close = 0;

      cursor = foreground;
      cursor_text_color = background;
      url_color = color4;
      url_style = "single";
      foreground = foreground;
      background = background;
      selection_foreground = foreground;
      selection_background = "#eedc82";
      color1 = color1;
      color2 = color2;
      color3 = color3;
      color4 = color4;
      color5 = color5;
      color6 = color6;
      color7 = color7;
      color8 = color8;
      color9 = color9;
      color10 = color10;
      color11 = color11;
      color12 = color12;
      color13 = color13;
      color14 = color14;
      color15 = color15;
    };

    # extraConfig = ''
    #   map ctrl+f launch --type=overlay --stdin-source=@screen_scrollback ${pkgs.fzf}/bin/fzf --no-sort --no-mouse --exact -i
    # '';
    extraConfig = ''
      map ctrl+f combine : show_scrollback : send_text normal,application /
      map ctrl+c copy_and_clear_or_interrupt
      map ctrl+v paste_from_clipboard
    '';
  };

  apps.terminal = {
    name = "kitty";
    command = "kitty";
    desktopFile = "kitty.desktop";
    package = pkgs.kitty;
  };

  # Browser
  programs.cory.firefox = {
    enable = true;
    changeColor = false;
  };

  # File Manager
  # programs.cory.dolphin.enable = true;
  # programs.cory.dolphin.config = import ../../config/dolphin/laptop.nix;
  environment.variables = {
    GTK_USE_PORTAL = "1";
    XDG_DESKTOP_PORTAL = "1";
  };

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.libsForQt5.xdg-desktop-portal-kde ];
  };

  home-manager.users.cory.dconf = {
    enable = true;
    settings = {
      "org/mate/caja/icon-view" = {
        default-use-tighter-layout = true;
        default-zoom-level = "larger";
        labels-beside-icons = false;
      };
      "org/mate/caja/list-view" = {
        default-column-order = ["name" "size" "type" "date_modified" "date_accessed" "date_created" "extension" "group" "where" "mime_type" "octal_permissions" "owner" "permissions" "size_on_disk"];
        default-visible-columns = ["name" "size" "type" "date_modified"];
        default-zoom-level = "smaller";
      };
      "org/mate/caja/preferences" = {
        always-use-location-entry = false;
        click-policy = "single";
      };
      "org/mate/caja/geometry" = {
        side-pane-view = "tree";
        start-with-location-bar = true;
        start-with-sidebar = true;
        start-with-status-bar = true;
        start-with-toolbar = true;
      };
    };
  };

  apps.fileManager = {
    name = "caja";
    command = "caja";
    desktopFile = "caja.desktop";
    package = pkgs.mate.caja;
  };

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
      name = "strawberry";
      command = "strawberry";
      desktopFile = "org.strawberrymusicplayer.strawberry.desktop";
      package = pkgs.strawberry;
    };
    archiver = {
      name = "ark";
      command = "ark";
      desktopFile = "org.kde.ark.desktop";
      package = pkgs.libsForQt5.ark;
    };
    launcher = {
      name = "emacs-run-launcher";
      command = "xdotool mousemove 1800 1200; emacsclient -e '(emacs-run-launcher)'";
      desktopFile = "emacsclient.desktop";
      package = pkgs.emacsGit;
    };
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
    css = builtins.readFile ../../config/discocss/skeuocord.theme.css;
  };

  # Shell
  programs.cory.bat.enable = true;
  programs.cory.neofetch.enable = true;
  programs.cory.zsh.enable = true;

  # Gestures
  services.cory.touchegg = {
    enable = true;
    config = ../../config/touchegg/fvwm.conf;
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
      system = {
        package = oxygenfonts;
        name = "Oxygen-Sans";
        size = 11;
      };
      monospace = {
        package = victor-mono;
        name = "Victor Mono";
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
    source = ../../config/conky/laptop;
    recursive = true;
  };

  environment.systemPackages = with pkgs; [

  ];

  home-manager.users.cory.home.packages = with pkgs; [

  ];
}
