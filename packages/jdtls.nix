{ pkgs, ... }:

self: super: {
  jdtls = self.stdenv.mkDerivation rec {
    name = "jdtls";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/bin/
      ln -s $src/bin/jdt-language-server $out/bin/jdtls
    '';
    src = pkgs.jdt-language-server;
   };
}
