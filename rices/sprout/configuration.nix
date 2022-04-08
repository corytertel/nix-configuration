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
      extraPackages = haskellPackages: [
        haskellPackages.xmonad
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
      ];
      config = ./xmonad.hs;
    };
  };

  environment = {
    etc."wallpaper.jpg".source = ./wallpapers/sprout.jpg;
    systemPackages = with pkgs; [
      xmobar
      xdotool
    ];
  };
}
