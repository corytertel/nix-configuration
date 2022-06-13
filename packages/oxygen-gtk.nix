self: super: {
  oxygen-gtk = self.stdenv.mkDerivation {
    name = "oxygen-gtk";

    src = self.fetchFromGitHub {
      owner = "KDE";
      repo = "oxygen-gtk";
      rev = "3b0558f811d3dbf56de79939bfc1f6fee743fd41";
      sha256 = "ySL1zcf+JfF0Q9RzPmVjbL5NJXasCRqsG/0XQQNujR4=";
    };

    buildInputs = with self; [
      gtk
      cmake
    ];

    enableParallelBuilding = true;

    buildPhase = ''

    '';

    installPhase = ''
      mkdir -p $out/share/themes
      cp -r $src $out/share/themes
    '';
  };
}
