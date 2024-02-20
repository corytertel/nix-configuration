{ lib, pkgs, ... }:

self: super: {
  librecode = self.stdenv.mkDerivation {
    pname = "librecode";
    # version = "0.3";
    version = "0.4";
    dontBuild = true;

    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "librecode";
      # rev = "081ba63b3451e263c26cbe01755fc2fa8772c839";
      # sha256 = "sha256-6WeLByK10Vb0Jrp1UBKUqmdDHmGMiHO9hZxrHANOdn4=";
      rev = "fdde198697cf30894abb6ba0d06905be29b19cb5";
      hash = "sha256-tMhOjak5birPab9ogfnVl2aYRbN/9erWdNZhxAbd7y0=";
    };

    phases = ["installPhase"];

    installPhase = ''
      mkdir -p $out/share/fonts/truetype/
      cp $src/*.ttf $out/share/fonts/truetype/
    '';

    meta = with lib; {
      description = "A derivative of Liberation Serif for programming ";
      homepage = "https://github.com/corytertel/librecode";
      license = licenses.ofl;
      maintainers = with maintainers; [];
      platforms = platforms.linux;
    };
  };
}
