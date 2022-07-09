{ lib, pkgs, ... }:

self: super: {
  crystal-nova-icon-theme = self.stdenv.mkDerivation rec {
    pname = "crystal-nova-icon-theme";
    version = "1.0";

    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "crystal-nova-icon-theme";
      rev = "9f0f8eb9631197a12fbd3dd7e5cd0cff6358cccc";
      sha256 = "kPpOlcgQ9EI1P3EkzhJJ0PWMl7rGe4kMlVg4JFhHJ24=";
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
      mkdir -p $out/share/icons/crystal-nova
      cp -r $src/* $out/share/icons/crystal-nova
      for theme in $out/share/icons/*; do
        gtk-update-icon-cache -f $theme
      done
      runHook postInstall
    '';

    meta = with lib; {
      description = "The best of Nova7 and Crystal Remix mixed";
      homepage = "https://github.com/corytertel/crystal-nova-icon-theme";
      license = with licenses; [ lgpl3Only ];
      platforms = platforms.linux;
      maintainers = with maintainers; [ ];
    };
  };
}
