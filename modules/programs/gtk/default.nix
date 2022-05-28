{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.gtk;
in {
  options.programs.cory.gtk = {
    enable = mkEnableOption "Enables GTK and QT theming through Nix";
    minimal = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    programs.dconf.enable = true;
    qt5 = {
      enable = true;
      platformTheme = "gtk2";
      style = "gtk2";
    };

    home-manager.users.cory = {
      dconf.enable = true;

      gtk = {
        enable = true;

        font = {
          package = null;
          name = "NotoSans Nerd Font 10";
        };

        gtk2.extraConfig = ''
        gtk-key-theme-name = "Emacs"
      '';

        gtk3.extraConfig = { gtk-key-theme-name = "Emacs"; };

        theme = {
          package = pkgs.plainlight-gtk-theme;
          name = "PlainLight";
        };

        iconTheme = {
          package = pkgs.tango-icon-theme;
          name = "Tango";
        };

        gtk3.extraConfig = {
          gtk-icon-theme-name = "Tango";
          gtk-theme-name = "PlainLight";
          gtk-application-prefer-dark-theme = 0;
        };
      };

      qt = {
        enable = true;
        platformTheme = "gtk";
        style.name = "gtk2";
      };

      home.packages = with pkgs; [
        gnome.gnome-themes-extra
        gsettings-desktop-schemas
        libsForQt5.qtstyleplugins
      ];
    };
  };
}
