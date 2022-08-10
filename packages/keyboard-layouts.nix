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
      rev = "cf92ed636e3dfab2ebdcc3649af186b841a9d645";
      sha256 = "MUi9WeuraD4kE9vKBsaDG1zbOh3lzbsgvxVIB7NjZwE=";
    };
  };
}
