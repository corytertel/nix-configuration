{ pkgs, ... }:

super: self: {
  nixos-test = pkgs.writeShellScriptBin "nixos-test" ''
    nixos-rebuild test --flake .#$1 --use-remote-sudo
  '';

  nixos-switch = pkgs.writeShellScriptBin "nixos-switch" ''
    nixos-rebuild switch --flake .#$1 --use-remote-sudo
  '';
}
