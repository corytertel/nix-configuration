#!/bin/sh
pushd ~/.nix-configuration
# Home Manager
nix build .#homeManagerConfigurations.pc.activationPackage --show-trace
./result/activate
# System
#sudo nixos-rebuild switch --flake .#pc
doas nixos-rebuild switch --flake .#pc
popd
