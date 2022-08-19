{ pkgs }:

super: self: {

  chicken-lsp-server = let
    stdenv = pkgs.stdenv;
    eggs = import ./eggs.nix { inherit pkgs stdenv; };
  in
    pkgs.symlinkJoin {
      name = "chicken-lsp-server";
      paths = with eggs; [
        pkgs.chicken
        lsp-server

        # lsp-server dependencies
        apropos
        chicken-doc
        srfi-18

        # dependencies of the dependencies
        check-errors
        comparse
        fmt
        iset
        json-rpc
        lazy-seq
        matchable
        medea
        miscmacros
        nrepl
        r7rs
        regex
        srfi-1
        srfi-13
        srfi-130
        srfi-14
        srfi-141
        srfi-69
        string-utils
        sxml-transforms
        symbol-utils
        trie
        utf8
        vector-lib
      ];
    };

}
