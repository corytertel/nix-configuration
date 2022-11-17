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
      rev = "fa0fcfcef1355b2f7b0b0a024ea6eb4d68e51ec4";
      sha256 = "jk3W4SeadCSWMlh/dHRlASP5SFDDGFvOw4NbZ4FOC7M=";
    };
  };
}
