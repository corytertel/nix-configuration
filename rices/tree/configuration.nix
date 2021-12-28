{ config, pkgs, ... }:

{
  services.xserver = {
    # LightDM
    displayManager = {
      defaultSession = "none+fvwm";
      lightdm.greeters.mini = {
        enable = true;
        user = "cory";
        extraConfig = ''
          [greeter-theme]
          background-image = "/etc/wallpaper.jpg"
          background-color = "#0f0f0f"
          text-color = "#f0f0f0"
          error-color = "#f0f0f0"
          password-color = "#f0f0f0"
          password-background-color = "#0f0f0f"
          window-color = "#0f0f0f"
          border-color = "#f0f0f0"
        '';
      };
    };

    # fvwm
    windowManager.fvwm = {
      enable = true;
      gestures = true;
    };
  };

  environment.etc."wallpaper.jpg".source = ./fvwm/images/wallpaper/tree.jpg;
}
