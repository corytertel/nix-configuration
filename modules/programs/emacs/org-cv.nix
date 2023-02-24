{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "org-cv";
  version = "1.0.0";

  commit = "395069b1309f8743e45fe2137a816ee0b5c03900";

  src = pkgs.fetchFromGitHub {
    owner = "Titan-C";
    repo = "org-cv";
    rev = "395069b1309f8743e45fe2137a816ee0b5c03900";
    sha256 = "+o1LAYMaORNxZWBpd0y/keHASsw8i6XnJsjv5AKfB0s=";
  };

  buildInputs = with pkgs; [
    emacs
    emacsPackages.cl-lib
    emacsPackages.org
  ];

  packageRequires = with pkgs; [
    emacsPackages.cl-lib
    emacsPackages.org
  ];

  recipe = pkgs.writeText "recipe" ''
    (ox-moderncv
    :repo "corytertel/org-cv"
    :fetcher github
    :files ("*.el"))
  '';

  meta = with lib; {
    inherit (pkgs.emacs.meta) platforms;
  };
}
