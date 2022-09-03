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
      rev = "d61689898bf04405108f535fb16503cd7186f37e";
      sha256 = "xuwtQImu424yHjqkfnxgaCGnmt766gvl4aiHHUZOn6g=";
    };
  };
}
