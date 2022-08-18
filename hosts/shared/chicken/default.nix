{ pkgs }:
let
  stdenv = pkgs.stdenv;
  eggs = import ./eggs.nix { inherit pkgs stdenv; };
in
pkgs.eggDerivation {
  src = ./.;

  name = "chicken-eggs";
  buildInputs = with eggs; [
    apropos
    chicken-doc
    srfi-1
    srfi-18
    lsp-server
  ];
}
