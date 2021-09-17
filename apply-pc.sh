#!/bin/sh
pushd ~/.nix-configuration
# Home Manager
nix build .#homeManagerConfigurations.cory.activationPackage --show-trace
./result/activate
# System
sudo nixos-rebuild switch --flake .#pc
popd
