{ config, lib, pkgs, ... }:

# TODO need to set the Qt icon theme

{
  fonts = {
    packages = with pkgs; [
      noto-fonts
      dejavu_fonts
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [
          "DejaVu Sans Mono"
          "Noto Sans Mono"
        ];
        sansSerif = [ "Noto Sans" ];
        serif = [ "Noto Serif" ];
      };
    };
  };

  environment.variables = {
    # Qt env vars
    QT_QPA_PLATFORMTHEME = "qt5ct";
    QT_STYLE_OVERRIDE = "fusion";
    # Gtk env vars
    GTK_THEME = "Adwaita";
    GTK2_RC_FILES = "/etc/gtk-2.0/gtkrc";
  };

  environment.profileRelativeSessionVariables =
    let
      qtVersions = with pkgs; [
        qt5
        qt6
      ];
    in
      {
        QT_PLUGIN_PATH = map (qt: "/${qt.qtbase.qtPluginPrefix}") qtVersions;
        QML2_IMPORT_PATH = map (qt: "/${qt.qtbase.qtQmlPrefix}") qtVersions;
      };

  environment.etc = let
    gtk2 = ''
      gtk-theme-name="Adwaita"
      gtk-icon-theme-name="Adwaita"
      gtk-font-name="Noto Sans 12"
      gtk-cursor-theme-name="Adwaita"
    '';
    gtk3and4 = ''
      [Settings]
      gtk-theme-name=Adwaita
      gtk-icon-theme-name=Adwaita
      gtk-font-name=Noto Sans 12
      gtk-cursor-theme-name=Adwaita
    '';
  in {
    "gtk-2.0/gtkrc".text = gtk2;
    "gtk-3.0/settings.ini".text = gtk3and4;
    "gtk-4.0/settings.ini".text = gtk3and4;
  };

  environment.systemPackages = with pkgs; [
    adwaita-icon-theme
    hicolor-icon-theme # fallback icons

    xdg-user-dirs

    # all configured in dconf
    mate.eom
    mate.caja
    mate.atril
    mate.engrampa
    vlc
    # mate.mate-terminal
    mate.mate-system-monitor
    mate.mate-power-manager
    upower
    mate.mate-media # volume control tray app
  ]
  ++ lib.optional config.services.flatpak.enable pkgs.kdePackages.flatpak-kcm;

  programs.dconf.enable = true;

  services.gvfs.enable = true; # needed for caja trash

  xdg = {
    icons = {
      enable = true;
      # fallbackCursorThemes = [ "breeze_cursors" ];
      fallbackCursorThemes = [ "Adwaita" ];
    };
    portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };
    mime.defaultApplications = let
      caja = "caja.desktop";
      eom = "eom.desktop";
      vlc = "vlc.desktop";
      atril = "atril.desktop";
      engrampa = "engrampa.desktop";
    in {
      "inode/directory" = caja;

      "image/bmp" = eom;
      "image/gif" = eom;
      "image/ico" = eom;
      "image/jpeg" = eom;
      "image/png" = eom;
      "image/svg" = eom;
      "image/svg+xml" = eom;
      "image/svg-xml" = eom;
      "image/webp" = eom;
      "image/xpm" = eom;

      # "audio/aac" = elisa;
      # "audio/mpeg" = elisa;
      # "audio/ogg" = elisa;
      # "audio/opus" = elisa;
      # "audio/wav" = elisa;
      # "audio/weba" = elisa;

      "video/mp4" = vlc;
      "video/mpeg" = vlc;
      "video/ogg" = vlc;
      "video/webm" = vlc;
      "video/x-msvideo" = vlc;
      "video/quicktime" = vlc;
      "video/x-matroska" = vlc;

      "application/pdf" = atril;

      "application/zip" = engrampa;
      "application/x-7z-compressed" = engrampa;
      "application/vnd.rar" = engrampa;
      "application/gzip" = engrampa;
      "application/x-gzip" = engrampa;
      "application/x-gtar" = engrampa;
      "application/x-tgz" = engrampa;
      "application/x-xz-compressed-tar" = engrampa;
      "application/tar" = engrampa;
      "application/tar+gzip" = engrampa;
      "application/x-tar+gzip" = engrampa;
    };
  };

  # We manage Qt ourselves in this file
  qt.enable = false;
}
