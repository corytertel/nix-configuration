{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.firefox;

  colors = {
    tabs = {
      unfocused = {
        bg = "#f2f7fd";
        fg = "#141404";
      };

      focused = {
        bg = "#c0daff";
        fg = "#141404";
      };
    };

    urlbar = {
      bg = "#f2f7fd";
      fg = "#141404";
      urls.fg = "#c0daff";
      separator = "#141404";
      selected = {
        bg = "#c0daff";
        fg = "#141404";
      };
    };

    sidebar = {
      bg = "#f8f8f8";
      fg = "#c0daff";
    };

    about-blank.bg = "#ffffff";
  };
in {
  options.programs.cory.firefox = {
    enable = mkEnableOption "Enables firefox";
    changeColor = mkOption {
      type = types.bool;
      default = true;
    };
    invertedColor = mkOption {
      type = types.str;
      default = config.theme.color.foreground;
    };
    secondaryColor = mkOption {
      type = types.str;
      default = config.theme.color.background-alt1;
    };
    windowColor = mkOption {
      type = types.str;
      default = config.theme.color.background;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.programs.firefox = {
      enable = true;
      extensions = import ./extensions.nix { inherit pkgs; };
      profiles."cory" = {
        bookmarks = import ./bookmarks.nix;
        settings = import ./settings.nix { inherit config lib; };

        userContent = ''
        @-moz-document url-prefix(about:blank) {
          * {
            background-color: ${colors.about-blank.bg} !important;
          }
        }
      '';

        userChrome = ''
        :root {
          --color-unfocused-tabs-bg: ${colors.tabs.unfocused.bg};
          --color-unfocused-tabs-fg: ${colors.tabs.unfocused.fg};

          --color-focused-tabs-bg: ${colors.tabs.focused.bg};
          --color-focused-tabs-fg: ${colors.tabs.focused.fg};

          --color-urlbar-bg: ${colors.urlbar.bg};
          --color-urlbar-fg: ${colors.urlbar.fg};
          --color-urlbar-popup-url: ${colors.urlbar.urls.fg};
          --color-urlbar-separator: ${colors.urlbar.separator};

          --color-urlbar-selected-bg: ${colors.urlbar.selected.bg};
          --color-urlbar-selected-fg: ${colors.urlbar.selected.fg};
          --color-urlbar-selected-popup_url: ${colors.urlbar.selected.fg};

          --color-sidebar-bg: ${colors.sidebar.bg};
          --color-sidebar-fg: ${colors.sidebar.fg};
        }
      ''
        + builtins.readFile ./colors.css
        # + builtins.readFile ./tabbar/debloat-tabbar.css
        + builtins.readFile ./tabbar/tabbar-layout.css
        + builtins.readFile ./tabbar/tabs-fill-available-width.css
        # + builtins.readFile ./tabbar/numbered-tabs.css
        + builtins.readFile ./tabbar/tab-close-button-always-on-hover.css
        + builtins.readFile ./tabbar/hide-tabs-with-one-tab.css
        + builtins.readFile ./navbar/debloat-navbar.css
        + builtins.readFile ./navbar/navbar-layout.css
        + builtins.readFile ./navbar/navbar-on-focus.css
        + builtins.readFile ./urlbar/debloat-urlbar.css
        + builtins.readFile ./urlbar/urlbar-layout.css
        + builtins.readFile ./urlbar/remove-megabar.css
        + builtins.readFile ./sidebar/debloat-sidebar.css
        + builtins.readFile ./sidebar/sidebar-layout.css
        + ''
        #PersonalToolbar {
          display: none !important;
        }
      '';
      };
    };
    apps.browser = {
      name = "firefox";
      command = "firefox";
      desktopFile = "firefox.desktop";
      package = pkgs.firefox;
    };
  };
}
