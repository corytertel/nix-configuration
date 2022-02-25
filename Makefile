.PHONY: pc laptop update

pc:
	nix build .#homeManagerConfigurations.pc.activationPackage --show-trace
	./result/activate
	sudo nixos-rebuild switch --flake .#pc

laptop:
	nix build .#homeManagerConfigurations.laptop.activationPackage --show-trace
	./result/activate
	sudo nixos-rebuild switch --flake .#laptop

update:
	nix flake update

clean:
	sudo nix-collect-garbage

superclean:
	sudo nix-collect-garbage -d
