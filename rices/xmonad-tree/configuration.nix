{ config, pkgs, ... }:

{
  services.xserver = {
    # LightDM
    displayManager = {
      defaultSession = "none+xmonad";
      #lightdm.enable = true;
      #lightdm.greeters.gtk = {
      #  enable = true;
      #  extraConfig = ''
      #    [greeter]
      #    background=/usr/share/wallpaper.jpg
      #  '';
      #};
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

    # Enable SDDM
    #displayManager.sddm.enable = true;

    # Xmonad
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad/xmonad.hs;
    };
  };
}
