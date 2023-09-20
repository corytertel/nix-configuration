{ pkgs }:

super: self: {
  chicken-pkgs =
    pkgs.symlinkJoin {
      name = "chicken-pkgs";
      paths = with pkgs.chickenPackages_5.chickenEggs; [
        pkgs.chicken

        # Eggs for geiser
        apropos # srfi-1, utf8, check-errors, symbol-utils, string-utils
        chicken-doc # deps: srfi-1, srfi-13, srfi-69, fmt, matchable, sxml-transforms
        check-errors
        fmt
        iset
        lazy-seq
        matchable # pattern matching
        miscmacros
        regex
        srfi-1
        srfi-13
        srfi-14
        srfi-18 # threading
        srfi-69 # hash-table library
        string-utils # deps: check-errors, miscmacros, srfi-1, srfi-13, srfi-69, utf8
        sxml-transforms
        symbol-utils # deps: utf8
        utf8

        # Other eggs
        # typed-records
        defstruct # idk

        coops # object-system; deps: matchable, record-variants
        r7rs
        records
        record-variants # optimized record variants
        srfi-63 # homogeneous and heterogeneous arrays; deps: records
        srfi-95 # sorting and merging
        srfi-113 # sets and bags
        srfi-133 # vector-library
        srfi-196 # range objects; deps: srfi-1, srfi-133, utf8, typed-records
        srfi-197 # pipeline operators
      ];
    };
}
