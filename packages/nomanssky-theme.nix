self: super: {
  nomanssky-theme = self.stdenv.mkDerivation rec {
    name = "nomanssky-theme";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/kwin
      cp -R $src/location_right/aurorae $out/share/kwin
    '';
    src = self.fetchFromGitHub {
      owner = "adhec";
      repo = "kde-aurorae-tweaks";
      rev = "df0ca3aa6b0c2775e44112952fef531b656241b6";
      sha256 = "Cok2Nu22VnlI58uzVT93FXAZ8KT/h6R5L8cmkfUBuxs=";
    };
  };
}
