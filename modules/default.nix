{ config, lib, pkgs, ... }:

{
  imports = [
    ./programs/bash
    ./programs/discord
    ./programs/firefox
    ./programs/gtk
    ./programs/layout_switch
    ./programs/neofetch
    ./programs/ungoogled-chromium
    ./programs/urxvt
    ./programs/zathura
    ./programs/zsh

    ./services/dunst
    ./services/picom
    ./services/plank
    ./services/rofi
    ./services/tint2

    ./theme

    ./windowManagers/fvwm-pc
    # ./windowManagers/xmonad-laptop
    # ./windowManagers/xmonad-pc
  ];
}
