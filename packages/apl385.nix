{ lib, pkgs, ... }:

super: self: {
  apl385 = self.stdenv.mkDerivation {
    pname = "apl385";
    version = "latest";

    src = pkgs.fetchzip {
      url = "https://apl385.com/fonts/apl385.zip";
      sha256 = "xT5KK7FY7zEcfWVz/nqHIRYWaS9fU2+5BCclt+YgIdw=";
    };

    installPhase = ''
      runHook preInstall

      install -Dm444 -t $out/share/fonts/truetype *.ttf

      runHook postInstall
    '';

    meta = {
      homepage = "https://apl385.com/fonts/";
      description = "APL385 Unicode font";
      license = lib.licenses.unlicense;
      maintainers = with lib.maintainers; [ ];
      platforms = lib.platforms.all;
    };
  };
}
