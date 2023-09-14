{ lib, pkgs, ... }:

self: super: {
  julia-mono-nerdfont = self.stdenv.mkDerivation {
    pname = "JuliaMono-Nerd-Font";
    version = (builtins.parseDrvName pkgs.julia-mono.name).version;

    nativeBuildInputs = [pkgs.nerd-font-patcher pkgs.julia-mono];

    phases = ["installPhase"];

    preInstall = ''
    mkdir -p $out/share/fonts/truetype && cd "$_"
  '';

    installPhase = ''
    runHook preInstall
    find ${pkgs.julia-mono}/share/fonts/truetype \
      -name \*.ttf \
      -exec ${pkgs.nerd-font-patcher}/bin/nerd-font-patcher --complete --quiet --no-progressbars {} \; \
      -exec ${pkgs.nerd-font-patcher}/bin/nerd-font-patcher --complete --use-single-width-glyphs --adjust-line-height --quiet --no-progressbars {} \;
  '';

    meta = with lib; {
      description = "A monospaced font for scientific and technical computing";
      homepage = "https://juliamono.netlify.app/";
      license = licenses.ofl;
      maintainers = with maintainers; [];
      platforms = platforms.linux;
    };
  };
}
