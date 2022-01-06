.PHONY: pc laptop update

pc:
	nix build .#homeManagerConfigurations.pc.activationPackage --show-trace
	./result/activate
	doas nixos-rebuild switch --flake .#pc

laptop:
	nix build .#homeManagerConfigurations.laptop.activationPackage --show-trace
	./result/activate
	doas nixos-rebuild switch --flake .#laptop

update:
	nix flake update

clean:
	doas nix-collect-garbage

superclean:
	doas nix-collect-garbage -d
