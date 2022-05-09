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
      rev = "56228ce50e1cd369fe07b4f9ed4909569c032faa";
      sha256 = "+lRBF2hbmtTdSFDUjszm6ZXOaOoOtMeeZVCJmWwq0C8=";
    };
  };
}
