{ pkgs, ... }:

self: super: {
  keyboard-layouts = self.stdenv.mkDerivation rec {
    name = "keyboard-layouts";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/X11/xkb/symbols
      cp -a $src/v4/* $out/share/X11/xkb/symbols/
      cp -a $src/v5/* $out/share/X11/xkb/symbols/
    '';
    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "keyboard-layouts";
      rev = "8d1f2d3423ac9876ae24d76fd0b44dd776456bf1";
      sha256 = "gPLb89Fj/7P/5esTGmpxsUtscH2YnSgI9jSd9PZ2IMA=";
    };
  };
}
