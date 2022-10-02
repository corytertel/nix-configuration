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
      rev = "3a6da871bd70f5e10c19ca865e5dbd4e132d2476";
      sha256 = "9Qftla3G69Accrx8HzIg6DRHCqu5DQscvw7gw9VUKOI=";
    };
  };
}
