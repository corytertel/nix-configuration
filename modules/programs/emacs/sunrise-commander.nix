{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "sunrise";
  version = "1.0.0";

  commit = "4b31af22d834e9dbd4ebb743312130fa5855a3d2";

  src = pkgs.fetchFromGitHub {
    owner = "corytertel";
    repo = "sunrise-commander";
    rev = "4b31af22d834e9dbd4ebb743312130fa5855a3d2";
    sha256 = "jIFHejXGyBH4xZeTsMts89I7XVKCPyeRo3P368iJIv8=";
  };

  buildInputs = with pkgs; [
    emacs
    emacsPackages.advice
    emacsPackages.cl-lib
  ];

  packageRequires = with pkgs; [
    emacsPackages.advice
    emacsPackages.cl-lib
  ];

  recipe = pkgs.writeText "recipe" ''
    (sunrise
    :repo "corytertel/sunrise-commander"
    :fetcher github
    :files ("*.el"))
  '';

  meta = with lib; {
    inherit (pkgs.emacs.meta) platforms;
  };
}
