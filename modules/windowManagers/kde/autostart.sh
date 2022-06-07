#!/bin/sh

touchegg
pcmanfm-qt --daemon-mode
sxhkd ~/.config/sxhkd/sxhkdrc
urxvtd --quiet
emacs --daemon


plasma-apply-lookandfeel --apply org.kde.oxygen
plasma-apply-colorscheme OxygenCold
plasma-apply-cursortheme Oxygen_White
plasma-apply-desktoptheme org.kde.oxygenKDE4
plasma-apply-wallpaperimage ~/Desktop/maui.jpg
