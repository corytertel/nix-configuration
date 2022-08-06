self: super: {
  kmonad = let
    kmonad-bin = self.fetchurl {
      # url = "https://github.com/kmonad/kmonad/releases/download/0.3.0/kmonad-0.3.0-linux";
      # sha256 = "4545b0823dfcffe0c4f0613916a6f38a0ccead0fb828c837de54971708bafc0b";
      url = "https://github.com/kmonad/kmonad/releases/download/0.4.1/kmonad-0.4.1-linux";
      sha256 = "sha256-g55Y58wj1t0GhG80PAyb4PknaYGJ5JfaNe9RlnA/eo8=";
    };
  in
    self.runCommand "kmonad" {}
      ''
        #!${self.stdenv.shell}
        mkdir -p $out/bin
        cp ${kmonad-bin} $out/bin/kmonad
        chmod +x $out/bin/*
      '';
}
