{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "macrursors";
  version = "1.0.0";

  commit = "5c0a5052118b8dd2c1787dba9b06cb0a1514c268";

  src = pkgs.fetchFromGitHub {
    owner = "corytertel";
    repo = "macrursors";
    rev = "5c0a5052118b8dd2c1787dba9b06cb0a1514c268";
    sha256 = "GTi0iS3300yZm/uu4KkN9+c3Pv1aTk5OpcKYgurlAuE=";
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
