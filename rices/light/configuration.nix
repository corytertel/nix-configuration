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
        haskellPackages.xmonad_0_17_0
        haskellPackages.xmonad-contrib_0_17_0
        haskellPackages.xmonad-extras_0_17_0
      ];
      config = ./xmonad.hs;
    };
  };

  environment = {
    etc."wallpaper.jpg".source = ./wallpapers/flowers.jpg;
    systemPackages = with pkgs; [
      xmobar
      xdotool
    ];
  };
}
