{ config, lib, pkgs, ... }:

{
  imports = [ ./shared.nix ];

  windowManagers.cory.kde.config =
    import ../../config/kde/shared.nix { inherit config; } // import ../../config/kde/laptop.nix;

  theme.cursor = {
    theme = "Oxygen_White";
    size = 48;
  };

  theme.wallpaper = ./wallpapers/maui.jpg;
}
