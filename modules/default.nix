{ config, lib, pkgs, ... }:

{
  imports = [
    ./programs/bash
    ./programs/bat
    ./programs/caja
    ./programs/discord
    ./programs/emacs
    ./programs/firefox
    ./programs/gtk
    ./programs/kitty
    ./programs/mpc-qt
    ./programs/qpdfview
    ./programs/sxiv
    ./programs/ungoogled-chromium
    ./programs/zathura
    ./programs/zsh

    ./services/dunst
    ./services/picom
    ./services/touchegg

    ./apps
    ./theme

    ./windowManagers/fvwm/laptop
    ./windowManagers/fvwm/pc
    ./windowManagers/xmonad/laptop
    ./windowManagers/xmonad/pc
  ];
}
