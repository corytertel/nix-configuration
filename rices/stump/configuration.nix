{ config, pkgs, ... }:

{
  services.xserver = {
    displayManager = {
      defaultSession = "none+stumpwm";
      gdm.enable = true;
    };

    windowManager.stumpwm.enable = true;
  };

  environment = {
    etc."wallpaper.jpg".source = ./wallpapers/sprout.jpg;
    systemPackages = with pkgs; [
      xmobar
      xdotool
    ];
  };
}
