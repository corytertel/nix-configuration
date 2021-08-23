#!/bin/sh
pushd ~/.nix-configuration
sudo nixos-rebuild switch --flake .#
popd
