#!/bin/sh
pushd ~/.nix-configuration
# Home Manager
nix build .#homeManagerConfigurations.laptop.activationPackage --show-trace
./result/activate
# System
#sudo nixos-rebuild switch --flake .#laptop
doas nixos-rebuild switch --flake .#laptop
popd
