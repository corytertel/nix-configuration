{ lib, pkgs, ... }:

self: super: {
  nova7-icon-theme = self.stdenv.mkDerivation rec {
    pname = "nova7-icon-theme";
    version = "3.0";

    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "nova7-icon-theme";
      rev = "e63fdd1a3f19179641414a88f7f036694a321051";
      sha256 = "qN/eRqXWj1XHCeUREwAZSqN0MnUJBnsfzdEFsuftk7o=";
    };

    nativeBuildInputs = [
      pkgs.gtk3
    ];

    propagatedBuildInputs = [
      pkgs.breeze-icons
    ];

    dontDropIconThemeCache = true;

    installPhase = ''
      runHook preInstall
      mkdir -p $out/share/icons/nova7
      cp -r $src/* $out/share/icons/nova7
      for theme in $out/share/icons/*; do
        gtk-update-icon-cache -f $theme
      done
      runHook postInstall
    '';

    meta = with lib; {
      homepage = "https://store.kde.org/p/1379986";
      license = with licenses; [ lgpl3Only ];
      platforms = platforms.linux;
      maintainers = with maintainers; [ ];
    };
  };
}
