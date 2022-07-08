self: super: {
  expose-glassy-right = self.stdenv.mkDerivation rec {
    name = "expose-glassy-right";

    dontBuild = true;

    installPhase = ''
      mkdir -p $out/share/aurorae/themes/ExposeGlassyRight
      cp -r $src/* $out/share/aurorae/themes/ExposeGlassyRight
    '';

    src = self.fetchFromGitHub {
      owner = "corytertel";
      repo = "ExposeGlassyRight";
      rev = "7eed466bad11384b8203cbff65933c1b3a19cb9d";
      sha256 = "SGdSZ6W7U3ySFanUMnn4l9SJxwKiMqg2SM/fqAtd30A=";
    };
  };
}
