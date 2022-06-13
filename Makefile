.PHONY: pc laptop update clean superclean

pc:
	nixos-rebuild switch --flake .#pc --use-remote-sudo

laptop:
	nixos-rebuild switch --flake .#laptop --use-remote-sudo

update:
	nix flake update

clean:
	sudo nix-collect-garbage --delete-older-than 7d

superclean:
	sudo nix-collect-garbage -d
