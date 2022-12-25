{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "macrursors";
  version = "1.0.0";

  commit = "e29c782a069f4d0ce584edef1af2d9d4508a96fe";

  src = pkgs.fetchFromGitHub {
    owner = "corytertel";
    repo = "macrursors";
    rev = "e29c782a069f4d0ce584edef1af2d9d4508a96fe";
    sha256 = "2E7PSOiUXjKJeA8yTSaYWqRKe+G7wyyVsMipeXHFuP4=";
  };

  buildInputs = with pkgs; [
    emacs
    emacsPackages.cl-lib
  ];

  packageRequires = with pkgs; [
    emacsPackages.cl-lib
  ];

  recipe = pkgs.writeText "recipe" ''
    (macrursors
    :repo "corytertel/macrursors"
    :fetcher github
    :files ("*.el"))
  '';

  meta = with lib; {
    inherit (pkgs.emacs.meta) platforms;
  };
}
