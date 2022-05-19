{ config, pkgs, ... }:

{
  services.xserver = {
    displayManager = {
      defaultSession = "none+fvwm";
      sddm = {
        enable = true;
        enableHidpi = true;
        theme = "mountain-light";
      };
    };

    windowManager.fvwm = {
      enable = true;
      gestures = false;
    };
  };

  environment = {
    # etc."wallpaper.jpg".source = ./wallpapers/mountain.jpg;
    systemPackages = with pkgs; [
      xdotool
      feh
      xorg.xwd
      sddm-mountain-light
    ];
  };
}
