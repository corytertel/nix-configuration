{ pkgs, ... }:

self: super: {
  keyboard-layouts = self.stdenv.mkDerivation rec {
    name = "keyboard-layouts";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/X11/xkb/symbols
      cp -a $src/v3/* $out/share/X11/xkb/symbols/
    '';
    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "keyboard-layouts";
      rev = "c9339e9c8cf653db2345e023b64b8742fb07512b";
      sha256 = "nUtoc6KytGiaSksmf50GYrulugt9ZzxDSs2yzzy44dg=";
    };
  };
}
