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
          background-image = "/usr/share/wallpaper.jpg"
          background-color = "#f0f0f0"
          text-color = "#0f0f0f"
          error-color = "#0f0f0f"
          password-color = "#0f0f0f"
          password-background-color = "#f0f0f0"
          window-color = "#f0f0f0"
          border-color = "#0f0f0f"
        '';
      };
    };

    # fvwm
    windowManager.fvwm.enable = true;
  };
}
