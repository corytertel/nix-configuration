{ config, pkgs, ... }:

{
  services.xserver = {
    # LightDM
    displayManager = {
      defaultSession = "none+xmonad";
      lightdm.greeters.mini = {
        enable = true;
        user = "cory";
        extraConfig = ''
          [greeter-theme]
          background-image = "$HOME/Pictures/wallpaper.jpg";
          background-color = "#0C0F12"
          text-color = "#ff79c6"
          password-background-color = "#1E2029"
          window-color = "#181a23"
          border-color = "#bd93f9"
        '';
      };
    };

    # Enable SDDM
    #displayManager.sddm.enable = true;

    # Xmonad
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad/xmonad.hs;
    };
  };
}
