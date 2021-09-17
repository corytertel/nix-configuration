#!/bin/sh
pushd ~/.nix-configuration
cp /etc/nixos/hardware-configuration.nix system/hardware-configuration.nix
popd

pushd ~
nix-channel --add https://github.com/nix-community/home-manager/archive/release-21.05.tar.gz home-manager
nix-channel --update
echo "Please restart and then run 'nix-shell '<home-manager>' -A install' to complete the home-manager installation"
popd
