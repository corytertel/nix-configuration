{ pkgs, ... }:

self: super: {
  sddm-oxygen = self.stdenv.mkDerivation rec {
    name = "sddm-oxygen-theme";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/sddm/themes
      cp -aR $src $out/share/sddm/themes/oxygen
    '';
    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "sddm-oxygen";
      rev = "21ce729bc46c5ddad88a924231a7840d64420792";
      sha256 = "GYAw5jtzKAPVlrjrDveS6TLvvr94CGbqsZsdBUePsxc=";
    };
  };
}
