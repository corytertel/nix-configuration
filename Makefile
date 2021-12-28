.PHONY: pc laptop update

pc:
	if [ "$(git diff --shortstat)" == "" ]; then \
		nix build .#homeManagerConfigurations.pc.activationPackage --show-trace; \
		./result/activate; \
		doas nixos-rebuild switch --flake .#pc; \
	else \
		echo ""; \
		echo "Please commit your changes, then rebuild."; \
	fi

laptop:
	if [ "$(git diff --shortstat)" == "" ]; then \
		nix build .#homeManagerConfigurations.laptop.activationPackage --show-trace; \
		./result/activate; \
		doas nixos-rebuild switch --flake .#laptop; \
	else \
		echo ""; \
		echo "Please commit your changes, then rebuild."; \
	fi

# Will update the packages for both the system and the user (home-manager)
update:
	nix flake update
