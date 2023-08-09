{ pkgs }:

super: self: {
  chicken-pkgs =
    pkgs.symlinkJoin {
      name = "chicken-pkgs";
      paths = with pkgs.chickenPackages_5.chickenEggs; [
        pkgs.chicken

        apropos
        chicken-doc
        check-errors
        fmt
        iset
        lazy-seq
        miscmacros
        r7rs
        regex
        string-utils
        sxml-transforms
        symbol-utils
        trie
        utf8
        vector-lib
        imlib2

        coops
        matchable
        record-variants

        srfi-1
        srfi-13
        srfi-14
        srfi-18 # threading
        srfi-25 # multi-dimensional arrays
        srfi-34 # exception handling
        srfi-47 # arrays
        srfi-63 # homogeneous and heterogeneous arrays
        srfi-69 # hash-table library
        srfi-78 # lightweight testing
        srfi-95 # sorting and merging
        srfi-99 # record types
        srfi-113 # sets and bags
        srfi-130
        srfi-141
        srfi-158 # generators and accumulators
        srfi-193 # command line
        srfi-196 # range objects
        srfi-197 # pipeline operators
        srfi-203 # sicp programmatic drawing of pictures
        srfi-216 # sicp prerequisites
      ];
    };
}
