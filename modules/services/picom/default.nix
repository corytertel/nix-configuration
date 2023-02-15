{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.services.cory.picom;
in {
  options.services.cory.picom = {
    enable = mkEnableOption "Enables picom";
    package = mkOption {
      type = types.package;
      default = pkgs.picom;
    };
    roundBorders = mkOption {
      type = types.bool;
      default = false;
    };
    cornerRadius = mkOption {
      type = types.int;
      default = 0;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.services.picom = {
      enable = true;
      package = cfg.package;

      # inactiveDim = "0.02";

      backend = "glx";
      vSync = true;

      fade = false;

      opacityRules = [
        # "93:class_g = 'Emacs' && !_NET_WM_STATE@:32a" "0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
        # "70:class_g *?= 'FvwmButtons'"
        # "70:class_g *?= 'Conky'"
      ];

      settings = {
        # glx-no-stencil = true;
        glx-no-rebind-pixmap = true;
        unredir-if-possible = true;

        corner-radius = cfg.cornerRadius;
        round-borders = if cfg.roundBorders then 1 else 0;
        rounded-corners-exclude = [
          "class_g *?= 'FvwmButtons'"
          "class_g *?= 'Conky'"
        ];

        shadow = true;
        shadow-radius = 20;
        shadow-opacity = 0.80;
        shadow-offset-x = -20;
        shadow-offset-y = -10;
        shadow-exclude = [
          "class_g *?= 'FvwmButtons'"
          "class_g *?= 'Conky'"
        ];
        clip-shadow-above  = [
          "class_g *?= 'FvwmButtons'"
          "class_g *?= 'Conky'"
        ];
        shadow-red = 0.0;
        shadow-green = 0.0;
        shadow-blue = 0.2;

        # blur = {
        #   method = "kernel";
        #   kernel = "11,11,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0";
        # };
        # blur-background-exclude = [
        #   "class_g *?= 'peek'"
        # ];

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
        # transition-length = 150;
      };
  };
  };
}
