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
      rev = "b409b7c06474bce4c5721125fde9756eaaf7f24e";
      sha256 = "jkJx8Ubfsa+GnKkP8gbcgGlAm1XRNTkopFpbHt0Xon4=";
    };
  };
}
