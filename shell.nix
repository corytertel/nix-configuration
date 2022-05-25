{ pkgs }:

with pkgs;
mkShell {
  name = "flakeEnv";
  buildInputs = [ rnix-lsp ];
  shellHook = ''
    alias nrb="nixos-rebuild build --flake ."
    alias nrt="sudo nixos-rebuild test --flake ."
    alias nrs="sudo nixos-rebuild switch --flake ."

    alias build-pc="nixos-rebuild switch --flake .#pc --use-remote-sudo"
    alias build-laptop="nixos-rebuild switch --flake .#laptop --use-remote-sudo"
  '';
}
