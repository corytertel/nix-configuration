{ pkgs, ... }:

self: super: {
  keyboard-layouts = self.stdenv.mkDerivation rec {
    name = "keyboard-layouts";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/X11/xkb/symbols
      cp -a $src/v2/us_programmer $out/share/X11/xkb/symbols/us_programmer
      cp -a $src/v2/ru_programmer $out/share/X11/xkb/symbols/ru_programmer
    '';
    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "keyboard-layouts";
      rev = "89583a42d70cd74511b49096462a84209ce922f4";
      sha256 = "HC1QmffF+86+WHNAdflMHa9+ICC7aEufsxVLMhGeBlU=";
    };
  };
}
