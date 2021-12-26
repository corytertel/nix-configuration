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
          background-color = "#f0f0f0"
          text-color = "#0f0f0f"
          error-color = "#0f0f0f"
          password-color = "#0f0f0f"
          password-background-color = "#f0f0f0"
          window-color = "#f0f0f0"
          border-color = "#ac8a8c"
        '';
      };
    };

    # fvwm
    windowManager.fvwm.enable = true;
  };

  environment.etc."wallpaper.jpg".source = ./fvwm/images/wallpaper/beach.jpg;
}
