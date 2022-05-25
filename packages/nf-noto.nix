{ lib, pkgs, ... }:

self: super: {
  noto-nerdfont = self.stdenv.mkDerivation {
    pname = "NF-Noto";
    version = pkgs.noto-fonts.version;

    nativeBuildInputs = [pkgs.nerd-font-patcher pkgs.noto-fonts];

    phases = ["installPhase"];

    preInstall = ''
    mkdir -p $out/share/fonts/truetype && cd "$_"
  '';

    installPhase = ''
    runHook preInstall

    find ${pkgs.noto-fonts}/share/fonts/truetype \
      -name NotoSans-\* \
      -exec ${pkgs.nerd-font-patcher}/bin/nerd-font-patcher --complete --careful --removeligatures --quiet --no-progressbars {} \; \
      -exec ${pkgs.nerd-font-patcher}/bin/nerd-font-patcher --complete --careful --removeligatures --use-single-width-glyphs --adjust-line-height --quiet --no-progressbars {} \;

    find ${pkgs.noto-fonts}/share/fonts/truetype \
      -name NotoSerif-\* \
      -exec ${pkgs.nerd-font-patcher}/bin/nerd-font-patcher --complete --careful --removeligatures --quiet --no-progressbars {} \; \
      -exec ${pkgs.nerd-font-patcher}/bin/nerd-font-patcher --complete --careful --removeligatures --use-single-width-glyphs --adjust-line-height --quiet --no-progressbars {} \;

  '';

    meta = with lib; {
      description = "Beautiful and free fonts for many languages";
      homepage = "https://www.google.com/get/noto/";
      license = licenses.ofl;
      maintainers = with maintainers; [];
      platforms = platforms.linux;
    };
  };
}
