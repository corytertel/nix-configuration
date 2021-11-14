.PHONY: pc laptop update

pc:
	pushd ~/.nix-configuration && \
	nix build .#homeManagerConfigurations.pc.activationPackage --show-trace && \
	./result/activate && \
	doas nixos-rebuild switch --flake .#pc && \
	popd

laptop:
	pushd ~/.nix-configuration && \
	nix build .#homeManagerConfigurations.laptop.activationPackage --show-trace && \
	./result/activate && \
	doas nixos-rebuild switch --flake .#laptop && \
	popd

# Will update the packages for both the system and the user (home-manager)
update:
	nix flake update
