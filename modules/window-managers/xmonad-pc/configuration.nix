{ config, pkgs, ... }:

{
  services.xserver = {
    displayManager = {
      defaultSession = "none+xmonad";
      sddm = {
        enable = true;
        enableHidpi = true;
        theme = "mountain-light";
      };
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
      ];
      config = ./xmonad.hs;
    };
  };

  environment = {
    etc."wallpaper.jpg".source = ./wallpapers/mountain.jpg;
    systemPackages = with pkgs; [
      xdotool
      sddm-mountain-light
    ];
  };
}
