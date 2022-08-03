{ lib, pkgs, ... }:

self: super: {
  crystal-nova-icon-theme = self.stdenv.mkDerivation rec {
    pname = "crystal-nova-icon-theme";
    version = "1.0";

    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "crystal-nova-icon-theme";
      rev = "4c1499adc57033f7381ff96a94dac7380333e424";
      sha256 = "5BK6akyqacM3g1rOfBGkMwliL19W0yauGO5LC71HiRI=";
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
