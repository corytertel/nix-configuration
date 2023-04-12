{ config, lib, pkgs, ... }:

{
  imports = [
    ./programs/bash
    ./programs/bat
    ./programs/caja
    ./programs/discord
    ./programs/dolphin
    ./programs/emacs
    ./programs/firefox
    ./programs/gtk
    ./programs/gwenview
    ./programs/kitty
    ./programs/konsole
    ./programs/krusader
    ./programs/lximage-qt
    ./programs/mpc-qt
    ./programs/neofetch
    ./programs/qpdfview
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

    ./apps
    ./theme

    ./windowManagers/exwm
    ./windowManagers/fluxbox
    ./windowManagers/fvwm-laptop
    ./windowManagers/fvwm-pc
    ./windowManagers/kde
    # ./windowManagers/xmonad-laptop
    # ./windowManagers/xmonad-pc
    # ./windowManagers/xmonad
  ];
}
