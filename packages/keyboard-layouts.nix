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
      rev = "803e3e93484944d0ab3b18b44d491f47f11df854";
      sha256 = "O7EoINQpD3c6Ndxmo3d0OxEmuNpE657GhbJX+7KwGeY=";
    };
  };
}
