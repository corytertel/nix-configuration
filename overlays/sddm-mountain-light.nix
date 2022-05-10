{ pkgs, ... }:

self: super: {
  sddm-mountain-light = self.stdenv.mkDerivation rec {
    name = "sddm-mountain-light-theme";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/sddm/themes
      cp -aR $src $out/share/sddm/themes/mountain-light
    '';
    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "sddm-mountain-light";
      rev = "d7b5fd41f68b003ae4b88107b62d8265df5fdeaa";
      sha256 = "A/56OJ66EMvNindLJMwn9G4MTJ00Dpmlm+lZVutNerY=";
    };
  };
}
