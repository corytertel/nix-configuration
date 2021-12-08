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
          background-image = "/usr/share/wallpaper.jpg"
          background-color = "#282828"
          text-color = "#ebdbb2"
          error-color = "#ebdbb2"
          password-color = "#282828"
          password-background-color = "#ebdbb2"
          window-color = "#282828"
          border-color = "#ebdbb2"
        '';
      };
    };

    # Xmonad
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages : [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
      config = ./xmonad.hs;
    };
  };
}
