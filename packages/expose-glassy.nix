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
      rev = "0fae6066e6f69ed397ec7fb7d1f60902f5d3fd39";
      sha256 = "mRpCJ8rIY1XV96LM1hJI4XmNKeiF0bDHlAxfB119F+Y=";
    };
  };
}
