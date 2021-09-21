{ config, pkgs, ... }:

{
  imports =
    [
      ./apps/kitty
      ./apps/bash
    ];

  home.file = {
    ".Xresources".text = builtins.readFile ./system/Xresources;
  };
}
