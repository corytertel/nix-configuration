#!/bin/sh

# Extra install script to help install config files that are unable to be
# put on the nix store for whatever reason

RICE='~/.nix-configuration/rices/light';

# tint2
cp $RICE/tint2/tint2rc ~/.config/tint2/tint2rc

# firefox


# pcmanfm-qt
cp $RICE/pcmanfm-qt/settings.conf ~/.config/pcmanfm-qt/default/settings.conf

# desktop icons
