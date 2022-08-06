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
      rev = "162e26623b6a11d9cb4e2144f2489e3af973ffae";
      sha256 = "gunHukdExrK3C5wo5BnMN8rGnvzfTvemAs40l5cifT8=";
    };
  };
}
