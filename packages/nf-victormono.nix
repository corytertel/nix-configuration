{ lib, pkgs, ... }:

self: super: {
  victor-mono-nerdfont = self.stdenv.mkDerivation {
    pname = "NF-VictorMono";
    version = (builtins.parseDrvName pkgs.victor-mono.name).version;

    nativeBuildInputs = [pkgs.nerd-font-patcher pkgs.victor-mono];

    phases = ["installPhase"];

    preInstall = ''
    mkdir -p $out/share/fonts/truetype && cd "$_"
  '';

    installPhase = ''
    runHook preInstall
    find ${pkgs.victor-mono}/share/fonts/truetype \
      -name \*.ttf \
      -exec ${pkgs.nerd-font-patcher}/bin/nerd-font-patcher --complete --quiet --no-progressbars {} \; \
      -exec ${pkgs.nerd-font-patcher}/bin/nerd-font-patcher --complete --use-single-width-glyphs --adjust-line-height --quiet --no-progressbars {} \;
  '';

    meta = with lib; {
      description = "Free programming font with cursive italics and ligatures";
      homepage = "https://rubjo.github.io/victor-mono";
      license = licenses.ofl;
      maintainers = with maintainers; [];
      platforms = platforms.linux;
    };
  };
}
