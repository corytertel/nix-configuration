{ lib, pkgs, ... }:

self: super: {
  crystal-nova-icon-theme = self.stdenv.mkDerivation rec {
    pname = "crystal-nova-icon-theme";
    version = "1.0";

    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "crystal-nova-icon-theme";
      rev = "1f7f2a8c077b5c8c192d8eb844ac0c13e82abcfc";
      sha256 = "jaCa6SQiYHojiZOgD548Rh0nvIzM3DNsSa+Lm7FPoUw=";
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
