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
      rev = "53d8719c73b06087e100ab26d1b1391fa63edda3";
      sha256 = "xu5JOp0T9+LtknqsRKP4Jgfg1PHE8a6r7lxReyGRnNA=";
    };
  };
}
