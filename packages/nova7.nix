{ lib, pkgs, ... }:

self: super: {
  nova7-icon-theme = self.stdenv.mkDerivation rec {
    pname = "nova7-icon-theme";
    version = "3.0";

    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "nova7-icon-theme";
      rev = "1a72ef08c048e6f5ea33f755614b0f04dcc79ff8";
      sha256 = "R24MMQyIWa/wqFhkjvb9IlbzCR8tbBhSqMqJBbWLF2w=";
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
      description = "A theme rooted in the icon designs of the noughties.";
      homepage = "https://store.kde.org/p/1379986";
      license = with licenses; [ lgpl3Only ];
      platforms = platforms.linux;
      maintainers = with maintainers; [ ];
    };
  };
}
