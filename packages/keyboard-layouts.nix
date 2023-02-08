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
      rev = "d72df112aa67a4b6756aaba876386d1e3186bcc8";
      sha256 = "9lmqlRlq2P+1LQN4XOP6/D29R6LtzlPGy4e2d3O2SgI=";
    };
  };
}
