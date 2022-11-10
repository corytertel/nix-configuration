{ pkgs, ... }:

self: super: {
  keyboard-layouts = self.stdenv.mkDerivation rec {
    name = "keyboard-layouts";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/X11/xkb/symbols
      cp -a $src/v4/* $out/share/X11/xkb/symbols/
    '';
    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "keyboard-layouts";
      rev = "ff15f10743aa2cf42b6d2dcbd3cadfb015ba416e";
      sha256 = "dkkcY8x/FrtEbptcXnwD+V4frXnXrCt+oI8WSWWI+3g=";
    };
  };
}
