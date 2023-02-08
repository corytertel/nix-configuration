{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "macrursors";
  version = "1.0.0";

  commit = "5c0a5052118b8dd2c1787dba9b06cb0a1514c268";

  src = pkgs.fetchFromGitHub {
    owner = "corytertel";
    repo = "macrursors";
    rev = "d62b8b1ef24f8840fa2e33b4d48367e0cd61939e";
    sha256 = "FDoQef05Cxi5y7vj+K86UQme0DwaQiSqbX1eXn7AVms=";
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
