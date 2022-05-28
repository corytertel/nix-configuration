{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.cory.picom;
in {
  options.services.cory.picom = {
    enable = mkEnableOption "Enables picom";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.services.picom = {
      enable = true;
      experimentalBackends = false;

      inactiveDim = "0.02";

      backend = "glx";
      vSync = true;

      fade = true;
      fadeDelta = 5;
      fadeSteps = [ "0.03" "1.00" ];
      fadeExclude = [ "class_g != 'Rofi'" ];

      extraOptions = ''
        glx-no-stencil = true;
        glx-no-rebind-pixmap = true;
        unredir-if-possible = true;

        corner-radius = 10;
        round-borders = 1;

        shadow = true;
        shadow-radius = 40;
        shadow-opacity = 0.70;
        shadow-offset-x = -40;
        shadow-offset-y = -30;
        shadow-exclude = [
          "class_g *?= 'plank'",
        ];
        clip-shadow-above = [
          "class_g *?= 'FvwmButtons'",
          "class_g *?= 'FvwmPager'",
          "class_g *?= 'tint2'",
        ];

        blur-background-exclude = [
          "_GTK_FRAME_EXTENTS@:c",
          "window_type = 'dock'",
        ];

        blur:
        {
          method = "kernel";
          kernel = "11,11,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0";
        };

        wintypes:
        {
          dock = { shadow = true; };
          dnd = { shadow = true; };
        };

        mark-wmwin-focused = true;
        mark-ovredir-focused = true;
        detect-rounded-corners = true;
        detect-client-opacity = true;

        unredir-if-possible-exclude = [ ];
        detect-transient = true;
        detect-client-leader = true;

        invert-color-include = [ ];
        glx-no-stencil = true;
        use-damage = false;
        transparent-clipping = false;
      '';
    };
  };
}
