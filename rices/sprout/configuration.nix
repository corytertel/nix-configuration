{ config, pkgs, ... }:

{
  services.xserver = {
    displayManager = {
      defaultSession = "none+xmonad";
      gdm.enable = true;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };
  };

  environment.etc."wallpaper.jpg".source = ./wallpapers/sprout.jpg;
}
