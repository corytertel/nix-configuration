{ lib, pkgs, ... }:

self: super: {
  oxygen-nerdfont = self.stdenv.mkDerivation {
    pname = "NF-Oxygen";
    version = (builtins.parseDrvName pkgs.oxygenfonts.name).version;

    nativeBuildInputs = [pkgs.nerd-font-patcher pkgs.oxygenfonts];

    phases = ["installPhase"];

    preInstall = ''
    mkdir -p $out/share/fonts/truetype && cd "$_"
  '';

    installPhase = ''
    runHook preInstall
    find ${pkgs.oxygenfonts}/share/fonts/truetype \
      -name \*.ttf \
      -exec ${pkgs.nerd-font-patcher}/bin/nerd-font-patcher --complete --quiet --no-progressbars {} \; \
      -exec ${pkgs.nerd-font-patcher}/bin/nerd-font-patcher --complete --use-single-width-glyphs --adjust-line-height --quiet --no-progressbars {} \;
  '';

    meta = with lib; {
      description = "Desktop/gui font for integrated use with the KDE desktop";
      homepage = "https://github.com/vernnobile/oxygenFont";
      license = licenses.ofl;
      maintainers = with maintainers; [];
      platforms = platforms.all;
    };
  };
}
