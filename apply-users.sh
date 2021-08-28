#!/bin/sh
pushd ~/.nix-configuration
nix build .#homeManagerConfigurations.cory.activationPackage
./result/activate
popd
