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
      rev = "d58a4f39d0f7ff6fd32a01b4df3120ff24435e80";
      sha256 = "orjSSyrYcRh7o8RW9l80IDVyZLdDy0u2dk4n46/5rrk=";
    };
  };
}
