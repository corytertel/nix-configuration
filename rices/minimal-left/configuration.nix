{ config, pkgs, ... }:

{
  services.xserver = {
    # LightDM
    displayManager = {
      defaultSession = "none+fvwm";
      gdm.enable = true;
    };

    # fvwm
    windowManager.fvwm = {
      enable = true;
      gestures = true;
    };
  };

  environment.etc."wallpaper.jpg".source = ./fvwm/images/wallpaper/nixos-gruvbox.jpg;
}
