{ pkgs, ... }:

self: super: {
  keyboard-layouts = self.stdenv.mkDerivation rec {
    name = "keyboard-layouts";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/X11/xkb/symbols
      cp -a $src/v3/us_programmer $out/share/X11/xkb/symbols/us_programmer
      cp -a $src/v3/ru_programmer $out/share/X11/xkb/symbols/ru_programmer
    '';
    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "keyboard-layouts";
      rev = "b43d50417d14339eefe0e7ff1606019839dc8644";
      sha256 = "Kgn/AGFNAfz2/KNfpzMZNvUIhVov//5bPMuqLAvDePc=";
    };
  };
}
