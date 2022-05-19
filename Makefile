.PHONY: pc laptop update

pc:
	nix build .#homeManagerConfigurations.pc.activationPackage --show-trace
	./result/activate
	nixos-rebuild switch --flake .#pc --use-remote-sudo

laptop:
	nix build .#homeManagerConfigurations.laptop.activationPackage --show-trace
	./result/activate
	nixos-rebuild switch --flake .#laptop --use-remote-sudo

update:
	nix flake update

clean:
	sudo nix-collect-garbage

superclean:
	sudo nix-collect-garbage -d
