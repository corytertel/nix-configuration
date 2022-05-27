{ pkgs, ... }:

{
  home-manager.users.cory.services.picom = {
    enable = true;
    experimentalBackends = false;

    inactiveDim = "0.02";

    backend = "glx";
    vSync = true;

    fade = false;
    fadeDelta = 10;
    fadeSteps = [ (3.0e-2) (3.0e-2) ];

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
        "class_g *?= 'plank'",
      ];

      blur:
      {
        method = "kernel";
        kernel = "23,23,0.000006,0.000204,0.001698,0.003446,0.001698,0.000204,0.000006,0.000204,0.006988,0.058286,0.118212,0.058286,0.006988,0.000204,0.001698,0.058286,0.486234,0.986138,0.486234,0.058286,0.001698,0.003446,0.118212,0.986138,0.986138,0.118212,0.003446,0.001698,0.058286,0.486234,0.986138,0.486234,0.058286,0.001698,0.000204,0.006988,0.058286,0.118212,0.058286,0.006988,0.000204,0.000006,0.000204,0.001698,0.003446,0.001698,0.000204,0.000006";
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
    # package = pkgs.picom.overrideAttrs (
    #   o: {
    #     src = pkgs.fetchFromGitHub {
    #       # owner = "Arian8j2";
    #       # repo = "picom-jonaburg-fix";
    #       # rev = "31d25da22b44f37cbb9be49fe5c239ef8d00df12";
    #       # sha256 = "1z4bKDoNgmG40y2DKTSSh1NCafrE1rkHkCB3ob8ibm4=";
    #       owner = "jonaburg";
    #       repo = "picom";
    #       rev = "e3c19cd7d1108d114552267f302548c113278d45";
    #       sha256 = "4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y=";
    #     };
    #   }
    # );
  };
}
