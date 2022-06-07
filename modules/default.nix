{ config, lib, pkgs, ... }:

{
  imports = [
    ./programs/bash
    ./programs/discord
    ./programs/firefox
    ./programs/gtk
    ./programs/layout_switch
    ./programs/neofetch
    ./programs/sxiv
    ./programs/ungoogled-chromium
    ./programs/urxvt
    ./programs/zathura
    ./programs/zsh

    ./services/dunst
    ./services/libinput-gestures
    ./services/picom
    ./services/plank
    ./services/rofi
    ./services/sxhkd
    ./services/tint2
    ./services/touchegg

    ./theme

    ./windowManagers/fvwm-laptop
    ./windowManagers/fvwm-pc
    ./windowManagers/kde
    # ./windowManagers/xmonad-laptop
    # ./windowManagers/xmonad-pc
  ];
}
