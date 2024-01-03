
laptop:
	nixos-rebuild switch --flake .#laptop --use-remote-sudo

pc:
	nixos-rebuild switch --flake .#pc --use-remote-sudo

wsl:
	nixos-rebuild switch --flake .#wsl --use-remote-sudo

vm:
	nixos-rebuild switch --flake .#vm --use-remote-sudo

update:
	nix flake update

clean:
	sudo nix-collect-garbage --delete-older-than 7d

superclean:
	sudo nix-collect-garbage --delete-old

repair:
	sudo nix-store --verify --check-contents --repair
