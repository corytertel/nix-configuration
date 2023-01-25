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
      rev = "3792d8d3f253c8b388f34521ec42ba915a93c846";
      sha256 = "OVHsFngwZkZjnG/0LKiCzwlSiyrjA1GIXYU4XepbH1g=";
    };
  };
}
