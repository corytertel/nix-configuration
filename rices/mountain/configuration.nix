{ config, pkgs, ... }:

{
  services.xserver = {
    # LightDM
    displayManager.lightdm = {
      enable = true;
      greeters.enso.enable = true;
    };

    # Xmonad
    displayManager.defaultSession = "none+xmonad";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad/xmonad.hs;
    };
  };
}
