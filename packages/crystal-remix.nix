{ lib, pkgs, ... }:

self: super: {
  crystal-remix-icon-theme = self.stdenv.mkDerivation rec {
    pname = "crystal-remix-icon-theme";
    version = "1.1";

    src = builtins.fetchGit {
      url = "https://github.com/corytertel/crystal-remix-icon-theme.git";
      ref = "main";
      rev = "d7c6380065bdf0e7e066c551294d0ed35305310e";
    };

    nativeBuildInputs = [
      pkgs.gtk3
    ];

    propagatedBuildInputs = [
      pkgs.gnome.adwaita-icon-theme
      pkgs.breeze-icons
      pkgs.hicolor-icon-theme
    ];

    dontDropIconThemeCache = true;

    installPhase = ''
      runHook preInstall
      mkdir -p $out/share/icons/crystal-remix
      cp -r $src/* $out/share/icons/crystal-remix
      for theme in $out/share/icons/*; do
        gtk-update-icon-cache -f $theme
      done
      runHook postInstall
    '';

    meta = with lib; {
      description = "A crystal icon theme for modern Linux desktop environments.";
      homepage = "https://github.com/dangvd/crystal-remix-icon-theme";
      license = with licenses; [ lgpl2Only ];
      platforms = platforms.linux;
      maintainers = with maintainers; [ ];
    };
  };
}
