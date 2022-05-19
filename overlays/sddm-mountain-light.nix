{ pkgs, ... }:

self: super: {
  sddm-mountain-light = self.stdenv.mkDerivation rec {
    name = "sddm-mountain-light-theme";
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/share/sddm/themes
      cp -aR $src $out/share/sddm/themes/mountain-light
    '';
    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "sddm-mountain-light";
      rev = "d9582a3f99a192eee3458ce10e01a5a5a95f5c69";
      sha256 = "BcfeyVyb1Jvfmw+GUXIdUk29k7ZLoXZh2LVQOg+/MzQ=";
    };
  };
}
