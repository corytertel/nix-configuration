{ pkgs, stdenv }:
rec {
  inherit (pkgs) eggDerivation fetchegg fetchgit;

  apropos = eggDerivation {
    name = "apropos-3.7.0";

    src = fetchegg {
      name = "apropos";
      version = "3.7.0";
      sha256 = "1jsbs88ks3078iv3w56s56899cfmxxb69h9v09yj5mi24fg80m8i";
    };

    buildInputs = [
      srfi-1
      utf8
      string-utils
      symbol-utils
      check-errors
    ];
  };

  check-errors = eggDerivation {
    name = "check-errors-3.6.0";

    src = fetchegg {
      name = "check-errors";
      version = "3.6.0";
      sha256 = "0vn6x7a9h7d7isi4sngzlraav35pfigyyczf455r596wav7c132n";
    };

    buildInputs = [
      
    ];
  };

  chicken-doc = eggDerivation {
    name = "chicken-doc-0.7.0";

    src = fetchegg {
      name = "chicken-doc";
      version = "0.7.0";
      sha256 = "1vrk708444b79b7p8lpqzmvpzc326xag0azkys5dr72ng7550vdd";
    };

    buildInputs = [
      matchable
      fmt
      sxml-transforms
      srfi-1
      srfi-13
      srfi-69
    ];
  };

  comparse = eggDerivation {
    name = "comparse-3";

    src = fetchegg {
      name = "comparse";
      version = "3";
      sha256 = "0xc6sa1690h4mfbgv82pmcildyk8y2cgfma8ih2gjkjwbm28p4c2";
    };

    buildInputs = [
      lazy-seq
      trie
      matchable
      srfi-1
      srfi-13
      srfi-14
      srfi-69
    ];
  };

  fmt = eggDerivation {
    name = "fmt-0.8.11.2";

    src = fetchegg {
      name = "fmt";
      version = "0.8.11.2";
      sha256 = "0xvq2v3zqyl02gankmjyr6m50pwnj12byh7d3rrhmnr8xpin09gp";
    };

    buildInputs = [
      srfi-1
      srfi-13
      srfi-69
      utf8
    ];
  };

  iset = eggDerivation {
    name = "iset-2.2";

    src = fetchegg {
      name = "iset";
      version = "2.2";
      sha256 = "0yfkcd07cw6xnnqfbbvjy81x0vc54k40vdjp2m7gwxx64is6m3rz";
    };

    buildInputs = [
      
    ];
  };

  json-rpc = eggDerivation {
    name = "json-rpc-0.2.8";

    src = fetchegg {
      name = "json-rpc";
      version = "0.2.8";
      sha256 = "03pp1wh5c75spc2cpwb844hkggrnnigb35k3fzjwirfr42hc530j";
    };

    buildInputs = [
      medea
      r7rs
      srfi-1
      srfi-18
      srfi-69
    ];
  };

  lazy-seq = eggDerivation {
    name = "lazy-seq-2";

    src = fetchegg {
      name = "lazy-seq";
      version = "2";
      sha256 = "07d0v97r49f49ic2y73jyqcqj67z5zgaifykafd9fclxraff4s3s";
    };

    buildInputs = [
      srfi-1
    ];
  };

  lsp-server = eggDerivation {
    # name = "lsp-server-0.1.13";
    name = "lsp-server-master";

    # src = fetchegg {
    #   name = "lsp-server";
    #   version = "0.1.13";
    #   sha256 = "175rdw7a21f2f4y904g58fabfdwhwzg7vfjzlfr1jy8npjmcnwyx";
    # };
    src = stdenv.mkDerivation {
      name = "scheme-lsp-server";
      dontBuild = true;
      src = fetchgit {
        url = "https://codeberg.org/rgherdt/scheme-lsp-server";
        rev = "eb01611e165a64af86e52bf1968eaf13b338fa49";
        sha256 = "sha256-ejsHWqXmQq3nPPMaIjjXPUfvCuwMDkBSXn0RvffcuAI=";
      };
      installPhase = ''
        mkdir -p $out
        cp -r $src/* $out/
        chmod 755 -R $out
      '';
    };

    buildInputs = [
      apropos
      chicken-doc
      json-rpc
      nrepl
      r7rs
      srfi-1
      srfi-130
      srfi-18
      srfi-69
      utf8
      vector-lib
    ];
  };

  matchable = eggDerivation {
    name = "matchable-1.1";

    src = fetchegg {
      name = "matchable";
      version = "1.1";
      sha256 = "084hm5dvbvgnpb32ispkp3hjili8z02hamln860r99jx68jx6j2v";
    };

    buildInputs = [
      
    ];
  };

  medea = eggDerivation {
    name = "medea-4";

    src = fetchegg {
      name = "medea";
      version = "4";
      sha256 = "1rr8blml4j9xsgrw4cxjhz8c241vd9zrhp9d6mgc2sdjgcw1kl6v";
    };

    buildInputs = [
      comparse
      srfi-1
      srfi-13
      srfi-14
      srfi-69
    ];
  };

  miscmacros = eggDerivation {
    name = "miscmacros-1.0";

    src = fetchegg {
      name = "miscmacros";
      version = "1.0";
      sha256 = "0n2ia4ib4f18hcbkm5byph07ncyx61pcpfp2qr5zijf8ykp8nbvr";
    };

    buildInputs = [
      
    ];
  };

  nrepl = eggDerivation {
    name = "nrepl-5.0.8";

    src = fetchegg {
      name = "nrepl";
      version = "5.0.8";
      sha256 = "09wc22k2fvkdx7h88a3l019z8ffjx4fx29jwx70n78wxa0xhhf6d";
    };

    buildInputs = [
      srfi-18
    ];
  };

  r7rs = eggDerivation {
    name = "r7rs-1.0.5";

    src = fetchegg {
      name = "r7rs";
      version = "1.0.5";
      sha256 = "0zyi1z4m1995hm2wfc5wpi8jjgxcwk03qknq5v2ygff3akxazsf6";
    };

    buildInputs = [
      matchable
      srfi-1
      srfi-13
    ];
  };

  regex = eggDerivation {
    name = "regex-2.0";

    src = fetchegg {
      name = "regex";
      version = "2.0";
      sha256 = "0qgqrrdr95yqggw8xyvb693nhizwqvf1fp9cjx9p3z80c4ih8j4j";
    };

    buildInputs = [
      
    ];
  };

  srfi-1 = eggDerivation {
    name = "srfi-1-0.5.1";

    src = fetchegg {
      name = "srfi-1";
      version = "0.5.1";
      sha256 = "15x0ajdkw5gb3vgs8flzh5g0pzl3wmcpf11iimlm67mw6fxc8p7j";
    };

    buildInputs = [
      
    ];
  };

  srfi-13 = eggDerivation {
    name = "srfi-13-0.3.2";

    src = fetchegg {
      name = "srfi-13";
      version = "0.3.2";
      sha256 = "137yv5hkhnapgh7iq3siiq1vfqdmvck4gsax7p6pmch6adg2wcy4";
    };

    buildInputs = [
      srfi-14
    ];
  };

  srfi-130 = eggDerivation {
    name = "srfi-130-1.0";

    src = fetchegg {
      name = "srfi-130";
      version = "1.0";
      sha256 = "03z8lks2g2fq5bz4l33rdmkpx8gdvrgmfvwhb5qzzkipc4kym7zw";
    };

    buildInputs = [
      srfi-1
      utf8
      srfi-141
    ];
  };

  srfi-14 = eggDerivation {
    name = "srfi-14-0.2.1";

    src = fetchegg {
      name = "srfi-14";
      version = "0.2.1";
      sha256 = "0gc33cx4xll9vsf7fm8jvn3gc0604kn3bbi6jfn6xscqp86kqb9p";
    };

    buildInputs = [
      
    ];
  };

  srfi-141 = eggDerivation {
    name = "srfi-141-0.2";

    src = fetchegg {
      name = "srfi-141";
      version = "0.2";
      sha256 = "1c2hfzm6x93lb9l3slf7nnrjbk1p5w806miwnd7sz6sjbhssvlm4";
    };

    buildInputs = [
      
    ];
  };

  srfi-18 = eggDerivation {
    name = "srfi-18-0.1.6";

    src = fetchegg {
      name = "srfi-18";
      version = "0.1.6";
      sha256 = "00lykm5lqbrcxl3dab9dqwimpgm36v4ys2957k3vdlg4xdb1abfa";
    };

    buildInputs = [
      
    ];
  };

  srfi-69 = eggDerivation {
    name = "srfi-69-0.4.3";

    src = fetchegg {
      name = "srfi-69";
      version = "0.4.3";
      sha256 = "11pny54nc3rpmpaxcxs9dap1n6490y80zpwgfg0bwji1938a6fks";
    };

    buildInputs = [
      
    ];
  };

  string-utils = eggDerivation {
    name = "string-utils-2.6.0";

    src = fetchegg {
      name = "string-utils";
      version = "2.6.0";
      sha256 = "0nmw3gyi7z3svfqf9wjgy0p3sy6dia7ibynsm9mrzzssd7k39rk2";
    };

    buildInputs = [
      utf8
      srfi-1
      srfi-13
      srfi-69
      miscmacros
      check-errors
    ];
  };

  sxml-transforms = eggDerivation {
    name = "sxml-transforms-1.4.3";

    src = fetchegg {
      name = "sxml-transforms";
      version = "1.4.3";
      sha256 = "11hn1s2xf4nviskzp4pwqvfcx0nc6a3p8npvpvkfl3fv3i0l8cz7";
    };

    buildInputs = [
      srfi-13
    ];
  };

  symbol-utils = eggDerivation {
    name = "symbol-utils-2.3.0";

    src = fetchegg {
      name = "symbol-utils";
      version = "2.3.0";
      sha256 = "00v37a80irifk0gkz2wayii1blljzd2xrrf1ljmr7qqqnfn2c05j";
    };

    buildInputs = [
      utf8
    ];
  };

  trie = eggDerivation {
    name = "trie-2";

    src = fetchegg {
      name = "trie";
      version = "2";
      sha256 = "14pbwxn42ahq5vsfw36fmkhxd4kf86p6vhkbzd7529bafv135nwi";
    };

    buildInputs = [
      srfi-1
    ];
  };

  utf8 = eggDerivation {
    name = "utf8-3.6.3";

    src = fetchegg {
      name = "utf8";
      version = "3.6.3";
      sha256 = "1g41dlbnfgwkfllci7gfqfckfdz0nm5zbihs6vi3rdsjwg17g7iw";
    };

    buildInputs = [
      srfi-69
      iset
      regex
    ];
  };

  vector-lib = eggDerivation {
    name = "vector-lib-2.1.1";

    src = fetchegg {
      name = "vector-lib";
      version = "2.1.1";
      sha256 = "1402p9vqlqx6a0a704wzbk3zbnq26m24l7li9xp54hfr555hqdfh";
    };

    buildInputs = [
      
    ];
  };
}

