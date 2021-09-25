{ config, pkgs, ... }:

{
  imports =
    [
      ./apps/kitty
      ./apps/bash
    ];

  home.file = {
    ".Xresources".text = builtins.readFile ./system/Xresources;
    ".config/xmobar/xmobarrc".text = builtins.readFile ./system/xmobarrc;
  };
}
