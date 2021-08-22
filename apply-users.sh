#!/bin/sh
pushd ~/.nix-configuration
home-manager switch -f ./users/cory/home.nix
popd
