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
      # experimentalBackends = true;

      # package = pkgs.picom.overrideAttrs (o: {
      #   src = pkgs.fetchFromGitHub {
      #     owner = "jonaburg";
      #     repo = "picom";
      #     rev = "e3c19cd7d1108d114552267f302548c113278d45";
      #     sha256 = "4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y=";
      #   };
      # });

      # inactiveDim = "0.02";

      backend = "glx";
      vSync = true;

      fade = true;
      fadeDelta = 5;
      fadeSteps = [ 0.03 1.00 ];
      fadeExclude = [ "class_g != 'Rofi'" ];

      opacityRules = [
        # "93:class_g = 'Emacs' && !_NET_WM_STATE@:32a" "0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
        "70:class_g *?= 'FvwmButtons'"
      ];

      settings = {
        # glx-no-stencil = true;
        glx-no-rebind-pixmap = true;
        unredir-if-possible = true;

        corner-radius = 5;
        round-borders = 1;

        shadow = true;
        shadow-radius = 20;
        shadow-opacity = 0.80;
        shadow-offset-x = -20;
        shadow-offset-y = -10;

        blur = {
          method = "kernel";
          kernel = "11,11,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0";
        };

        wintypes = {
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

        # jonaburg
        transition-length = 150;
      };
  };
  };
}
