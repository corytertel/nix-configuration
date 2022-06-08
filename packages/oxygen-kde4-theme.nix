self: super: {
  oxygen-kde4-theme = self.stdenv.mkDerivation rec {
    name = "org.kde.oxygenKDE4";

    dontBuild = true;

    src = builtins.fetchGit {
      url = "https://github.com/MartinF99/Oxygen-KDE-4.git";
      ref = "master";
      rev = "9afe51f6ea66bbf9cf82b9d1a514d43e14c65f3f";
    };

    installPhase = ''
      mkdir -p $out/share/plasma/desktoptheme/org.kde.oxygenKDE4
      cp -r $src/* $out/share/plasma/desktoptheme/org.kde.oxygenKDE4
    '';
  };
}