# Credit to LunNova for supplying the original file
# I would have never been able to figure out KDE file patching

{ config, kdeConfig, lib, pkgs, ... }:

let
  dagEntryAfter = after: data: {
    inherit data after;
    before = [ ];
  };

  toValue = v:
    if builtins.isString v then
      v
    else if builtins.isBool v then
      lib.boolToString v
    else if builtins.isInt v then
      builtins.toString v
    else
      builtins.abort ("Unknown value type: " ++ builtins.toString v);

  lines = builtins.concatLists (builtins.map
    (configs: lib.flatten (lib.mapAttrsToList
      (file:
        lib.mapAttrsToList
          (group:
            lib.mapAttrsToList
              (key: value:
                "$DRY_RUN_CMD ${pkgs.libsForQt5.kconfig}/bin/kwriteconfig5 --file $confdir/'${file}' --group '${group}' --key '${key}' '${
                  toValue value
                }'")
          ))
      configs)) kdeConfig);

in {
  home.activation.kwriteconfig5 = dagEntryAfter [ "linkGeneration" ] ''
    _() {
      confdir="''${XDG_CONFIG_HOME:-$HOME/.config}"
      ${builtins.concatStringsSep "\n" lines}
      $DRY_RUN_CMD ${pkgs.libsForQt5.qt5.qttools.bin}/bin/qdbus org.kde.KWin /KWin reconfigure || echo "KWin reconfigure failed"
      for i in {0..10}; do
        $DRY_RUN_CMD ${pkgs.dbus}/bin/dbus-send --type=signal /KGlobalSettings org.kde.KGlobalSettings.notifyChange int32:$i int32:0 || echo "KGlobalSettings.notifyChange failed"
      done
    } && _
    unset -f _
  '';

  home.activation.extraGtkConfig = let
    updateConfig = key: line: file: ''
      grep -qF ${key} ${file} \
      && $DRY_RUN_CMD sed -i "s/^.*\b${key}\b.*$/${line}/g" ${file} \
      || $DRY_RUN_CMD echo '${line}' >> ${file}
    '';

    convertToGtk2 = with lib; value:
      let
        value' = if isBool value
                 then
                   boolToString value
                 else if isString value then
                   ''"${value}"''
                 else
                   toString value;
      in "${value'}";

    gtk2Configure = key: value: ''
      ${updateConfig key "${key}=${convertToGtk2 value}" "$HOME/.gtkrc-2.0"}
      ${updateConfig key "${key} = ${convertToGtk2 value}" "$HOME/.config/gtkrc-2.0"}
    '';

    gtk3Configure = key: value: ''
      ${updateConfig key "${key}=${toString value}" "$HOME/.config/gtk-3.0/settings.ini"}
    '';

    gtk4Configure = key: value: ''
      ${updateConfig key "${key}=${toString value}" "$HOME/.config/gtk-4.0/settings.ini"}
    '';

    font-name = "${config.theme.font.system.name},  ${toString config.theme.font.system.size}";

  in dagEntryAfter ["linkGeneration"] ''
    ${gtk2Configure "gtk-key-theme-name" "Emacs"}
    ${gtk3Configure "gtk-key-theme-name" "Emacs"}
    ${gtk4Configure "gtk-key-theme-name" "Emacs"}

    ${gtk2Configure "gtk-font-name" font-name}
    ${gtk3Configure "gtk-font-name" font-name}
    ${gtk4Configure "gtk-font-name" font-name}

    ${gtk2Configure "gtk-icon-theme-name" config.theme.icons.name}
    ${gtk3Configure "gtk-icon-theme-name" config.theme.icons.name}
    ${gtk4Configure "gtk-icon-theme-name" config.theme.icons.name}

    ${gtk2Configure "gtk-theme-name" "Breeze"}
    ${gtk3Configure "gtk-theme-name" "Breeze"}
    ${gtk4Configure "gtk-theme-name" "Breeze"}

    ${gtk2Configure "gtk-cursor-theme-name" config.theme.cursor.theme}
    ${gtk3Configure "gtk-cursor-theme-name" config.theme.cursor.theme}
    ${gtk4Configure "gtk-cursor-theme-name" config.theme.cursor.theme}
    ${gtk2Configure "gtk-cursor-theme-size" config.theme.cursor.size}
    ${gtk3Configure "gtk-cursor-theme-size" "${toString config.theme.cursor.size}"}
    ${gtk4Configure "gtk-cursor-theme-size" "${toString config.theme.cursor.size}"}

    ${gtk2Configure "gtk-enable-animations" 1}
    ${gtk2Configure "gtk-primary-button-warps-slider" 0}
    ${gtk2Configure "gtk-toolbar-style" 3}
    ${gtk2Configure "gtk-menu-images" 1}
    ${gtk2Configure "gtk-button-images" 1}

    ${gtk3Configure "gtk-application-prefer-dark-theme" "false"}
    ${gtk3Configure "gtk-button-images" "true"}
    ${gtk3Configure "gtk-decoration-layout" "icon:minimize,maximize,close"}
    ${gtk3Configure "gtk-enable-animations" "true"}
    ${gtk3Configure "gtk-menu-images" "true"}
    ${gtk3Configure "gtk-modules" "colorreload-gtk-module:window-decorations-gtk-module"}
    ${gtk3Configure "gtk-primary-button-warps-slider" "false"}
    ${gtk3Configure "gtk-toolbar-style" "3"}

    ${gtk4Configure "gtk-application-prefer-dark-theme" "false"}
    ${gtk4Configure "gtk-decoration-layout" "icon:minimize,maximize,close"}
    ${gtk4Configure "gtk-enable-animations" "true"}
    ${gtk4Configure "gtk-primary-button-warps-slider" "false"}
  '';

  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/interface" = with config.theme; {
        icon-theme = "${icons.name}";
        document-font-name = "${font.system.name} ${toString font.system.size}";
        font-name = "${font.system.name}, ${toString font.system.size}";
        monospace-font-name = "${font.monospace.name} ${toString font.monospace.size}";
        gtk-key-theme = "Emacs";

        avatar-directories = [];
        can-change-accels = false;
        clock-format = "24h";
        clock-show-date = true;
        clock-show-seconds = false;
        clock-show-weekday = false;
        cursor-blink = true;
        cursor-blink-time = 1200;
        cursor-blink-timeout = 10;
        cursor-size = config.theme.cursor.size;
        cursor-theme = cursor.theme;
        enable-animations = true;
        enable-hot-corners = true;
        font-antialiasing = "grayscale";
        font-hinting = "slight";
        font-rgba-order = "rgb";
        gtk-color-palette = "";
        gtk-enable-primary-paste = true;
        gtk-im-module = "";
        gtk-im-preedit-style = "callback";
        gtk-im-status-style = "callback";
        gtk-theme = "Breeze";
        gtk-timeout-initial = 200;
        gtk-timeout-repeat = 20;
        locate-pointer = false;
        menubar-accel = "F10";
        menubar-detachable = false;
        menus-have-tearoff = false;
        overlay-scrolling = true;
        scaling-factor = 0;
        show-battery-percentage = false;
        text-scaling-factor = 1;
        toolbar-detachable = false;
        toolbar-icons-size = "large";
        toolbar-style = "text";
        toolkit-accessibility = false;
      };
    };
  };

  home.activation.kdeSetTheme = dagEntryAfter [ "linkGeneration" ] ''
    $DRY_RUN_CMD ${pkgs.plasma-workspace}/bin/plasma-apply-colorscheme OxygenCory
    $DRY_RUN_CMD ${pkgs.plasma-workspace}/bin/plasma-apply-wallpaperimage ${config.theme.wallpaper}
  '';

  home.file =
    if config.windowManagers.cory.kde.rightWindowDecor
    then {
      ".local/share/kwin/aurorae" = {
        source = ./aurorae;
        recursive = true;
      };
    } else {} // {
      ".config/autostart" = {
        source = ../../../config/kde/autostart;
        recursive = true;
      };

      ".config/latte/Default.layout.latte".source = ../../../config/kde/Default.layout.latte;

      ".config/autostart/paplay.desktop".text = ''
        [Desktop Entry]
        Exec=paplay ${./KDE_Startup_1.ogg}
        Icon=
        Name=paplay
        Path=
        Terminal=False
        Type=Application
      '';
    };
}
