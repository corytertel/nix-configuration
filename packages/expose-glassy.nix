self: super: {
  expose-glassy = self.stdenv.mkDerivation rec {
    name = "expose-glassy";

    dontBuild = true;

    installPhase = ''
      mkdir -p $out/share/aurorae/themes/ExposeGlassy
      cp -r $src/* $out/share/aurorae/themes/ExposeGlassy
    '';

    src = self.fetchFromGitHub {
      owner = "corytertel";
      repo = "ExposeGlassy";
      rev = "630aec6457ba5095be39b09124d9dfd710efad66";
      sha256 = "Y5loKrNsiOT/DBWazmbzE82MBxqg57ruDug4xlT2tPA=";
    };
  };
}
