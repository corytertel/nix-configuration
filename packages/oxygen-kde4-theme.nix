self: super: {
  oxygen-kde4-theme = self.stdenv.mkDerivation rec {
    name = "org.kde.oxygenKDE4";

    dontBuild = true;

    src = builtins.fetchGit {
      # url = "https://github.com/MartinF99/Oxygen-KDE-4.git";
      url = "https://github.com/corytertel/Oxygen-KDE-4.git";
      ref = "master";
      # rev = "9afe51f6ea66bbf9cf82b9d1a514d43e14c65f3f";
      rev = "72f36788a6c85502b989e7c2238be3422df44175";
    };

    installPhase = ''
      mkdir -p $out/share/plasma/desktoptheme/org.kde.oxygenKDE4
      cp -r $src/* $out/share/plasma/desktoptheme/org.kde.oxygenKDE4
    '';
  };
}
