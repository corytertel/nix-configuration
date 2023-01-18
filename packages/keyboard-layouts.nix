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
      rev = "36605feded29d0cf45a8fa4bc683c7d99081730d";
      sha256 = "VviXa8ChgZ8+4KpUAcid1xP09pI7VhR91xeSIqXafoU=";
    };
  };
}
