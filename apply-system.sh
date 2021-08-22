#!/bin/sh
pushd ~/.nix-configuration
sudo nixos-rebuild switch -I nixos-config=./system/configuration.nix
popd
