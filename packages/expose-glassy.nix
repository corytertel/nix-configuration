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
      rev = "f003f18bbc3c0c7ac5887bf86ca51744a11333d4";
      sha256 = "pD0gUQaHoYB8WuJinAyOiH9RAnilU6GdjJkvL3WV3ew=";
    };
  };
}
