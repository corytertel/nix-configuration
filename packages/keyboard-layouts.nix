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
      rev =  "94558602019dc125af0914b6f52a63056360ecdf";
      sha256 =  "wHI8GFIc7tW8llyMQP+Y2K9KGcFP9652nP6Qq7ueQqc=";
    };
  };
}
