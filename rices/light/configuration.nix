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
          background-color = "#0f0f0f"
          text-color = "#f0f0f0"
          password-background-color = "#0f0f0f"
          window-color = "#0f0f0f"
          border-color = "#f0f0f0"
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
