{ config, pkgs, ... }:

{
  imports =
    [
      ./apps/kitty
    ];

  home.file = {
    ".Xresources".text = builtins.readFile ./system/Xresources;
  };
}
