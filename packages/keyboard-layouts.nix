{ pkgs, ... }:

self: super: {
  keyboard-layouts = self.stdenv.mkDerivation rec {
    name = "keyboard-layouts";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/X11/xkb/symbols
      cp -a $src/v3/us_programmer $out/share/X11/xkb/symbols/us_programmer
      cp -a $src/v3/ru_programmer $out/share/X11/xkb/symbols/ru_programmer
    '';
    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "keyboard-layouts";
      rev = "1305e76c87d547bf92c98a94d6cdbcfe3ee26d2b";
      sha256 = "foEqjX3Yimzf1Ove/OhP2duSlNCZV1rgR3o+WwAtIGk=";
    };
  };
}
