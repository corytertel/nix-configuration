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
      rev= "dbe4613f0d8b99a8b706c423411d2dc4ac367f58";
      sha256= "wd3yLH7rnr22+zDazHzKFGbCujJZJiUALrTcLZq/0xo=";
    };
  };
}
