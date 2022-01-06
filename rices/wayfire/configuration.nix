{ config, pkgs, lib, ... }:

{
  hardware = {
    nvidia.modesetting.enable = true;
    opengl.enable = true;
  };

  programs.xwayland.enable = true;

  fonts.enableDefaultFonts = true;
  programs.dconf.enable = true;

  # For screen sharing (this option only has an effect with xdg.portal.enable):
  xdg.portal.enable = true;
  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-wlr ];

  services.xserver = {
    displayManager = {
      #defaultSession = "none+fvwm";
      #sessionPackages = [ pkgs.wayfire ];
      gdm = {
        enable = true;
        wayland = true;
        nvidiaWayland = true;
      };
    };

    # fvwm
    windowManager.fvwm = {
      enable = true;
      gestures = false;
    };

    windowManager.session = [{
      name = "wayfire";
      #exec ${pkgs.wayfire}/bin/wayfire
      start = ''
        exec wayfire
      '';
    }];
  };

  environment = {
    etc."wallpaper.jpg".source = ./fvwm/images/wallpaper/mesa.jpg;
    systemPackages = with pkgs; [ wayfire wcm wf-config vanilla-dmz ];
    variables.WLR_NO_HARDWARE_CURSORS = "1";
  };
}
