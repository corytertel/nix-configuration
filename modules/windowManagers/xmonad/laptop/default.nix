{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.windowManagers.cory.xmonad-laptop;
in {
  options.windowManagers.cory.xmonad-laptop = {
    enable = mkEnableOption "Enable xmonad";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      displayManager = {
        defaultSession = "none+xmonad";
        sddm.enable = true;
      };

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
        ];
        config = ../../../../config/xmonad/laptop/xmonad.hs;
      };
    };

    programs.light.enable = true;

    environment.variables = {
      QT_AUTO_SCREEN_SCALE_FACTOR = "0";
      PLASMA_USE_QT_SCALING = "1";
      # FIXME very hacky solution
      GSETTINGS_SCHEMA_DIR = "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.pname}-${pkgs.gtk3.version}/glib-2.0/schemas:${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/gsettings-desktop-schemas-43.0/glib-2.0/schemas";
    };

    environment = {
      etc = {
        "wallpaper.jpg".source = ../../../../config/xmonad/laptop/images/maui.jpg;
        "xmobar/xmobarrc".source = ../../../../config/xmonad/laptop/xmobarrc;
        "xmobar/1.xpm".source = ../../../../config/xmonad/laptop/images/1.xpm;
        "xmobar/2.xpm".source = ../../../../config/xmonad/laptop/images/2.xpm;
        "xmobar/3.xpm".source = ../../../../config/xmonad/laptop/images/3.xpm;
        "xmobar/4.xpm".source = ../../../../config/xmonad/laptop/images/4.xpm;
        "xmobar/5.xpm".source = ../../../../config/xmonad/laptop/images/5.xpm;
        "xmobar/6.xpm".source = ../../../../config/xmonad/laptop/images/6.xpm;
        "xmobar/7.xpm".source = ../../../../config/xmonad/laptop/images/7.xpm;
        "xmobar/8.xpm".source = ../../../../config/xmonad/laptop/images/8.xpm;
        "xmobar/9.xpm".source = ../../../../config/xmonad/laptop/images/9.xpm;
        "xdg/jgmenu/jgmenurc".source = ../../../../config/xmonad/laptop/jgmenurc;
        "xdg/jgmenu/menu.csv".source = ../../../../config/xmonad/laptop/menu.csv;
      };
      systemPackages = let
        vol =
          pkgs.writeShellScriptBin "vol" (builtins.readFile ../../../../config/xmonad/laptop/scripts/vol);
        layout-switch =
          pkgs.writeShellScriptBin "layout-switch" (builtins.readFile ../../../../config/xmonad/laptop/scripts/layout-switch);
      in with pkgs; [
        vol
        layout-switch
        xmobar
        feh
        xorg.xwd
        xlockmore
	      trayer
        flameshot
        pavucontrol
        pasystray
        networkmanagerapplet
        cbatticon
        xdgmenumaker
        imagemagick
        kde-gtk-config
        trash-cli
        xdotool
        xscreensaver
        gtk3
        glib
        gsettings-desktop-schemas
        mate.mate-system-monitor
        mate.mate-power-manager
        upower
        mate.mate-media

        conky
        lua
        lm_sensors
        lsb-release

        jgmenu
      ];
    };
  };
}
