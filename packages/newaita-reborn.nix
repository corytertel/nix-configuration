{ lib, pkgs, ... }:

self: super: {
  newaita-reborn-icon-theme = self.stdenv.mkDerivation rec {
    pname = "newaita-reborn-icon-theme";
    version = "0.1";

    src = builtins.fetchGit {
      url = "https://github.com/cbrnix/Newaita-reborn.git";
      ref = "master";
      rev = "5b19f46a4ca918585038547b27810502a5997401";
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
      mkdir -p $out/share/icons
      mv Newaita-reborn* $out/share/icons/
      for theme in $out/share/icons/*; do
        gtk-update-icon-cache -f $theme
      done
      runHook postInstall
    '';

    meta = with lib; {
      description = "Remaster Newaita icon theme. Refreshed and made cleaner.";
      homepage = "https://github.com/cbrnix/Newaita-reborn";
      license = with licenses; [ gpl3Only ];
      platforms = platforms.linux;
      maintainers = with maintainers; [ ];
    };
  };
}
