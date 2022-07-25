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
      rev = "53cdc4ea369277a09a2ff6dac66ad43ec4fde812";
      sha256 = "TlF5ORb/FXeQS5YXFiiVYTo9RAVGRITR2kE0sl+PfWE=";
    };
  };
}
