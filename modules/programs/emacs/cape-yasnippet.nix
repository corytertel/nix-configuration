{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "cape-yasnippet";
  version = "1.0.0";

  commit = "ad8c27c2f748b410e7dd568927e99081f87b98b9";

  src = pkgs.fetchFromGitHub {
    owner = "elken";
    repo = "cape-yasnippet";
    rev = "ad8c27c2f748b410e7dd568927e99081f87b98b9";
    sha256 = "JmJljrvo9AcEVALv5GHa/XdpVLAUaww3wGmlXSAoITs=";
  };

  buildInputs = with pkgs; [
    emacs
    emacsPackages.cape
    emacsPackages.yasnippet
    emacsPackages.cl-lib
  ];

  packageRequires = with pkgs; [
    emacsPackages.cape
    emacsPackages.yasnippet
    emacsPackages.cl-lib
  ];

  recipe = pkgs.writeText "recipe" ''
    (cape-yasnippet
    :repo "elken/cape-yasnippet"
    :fetcher github
    :files ("*.el"))
  '';

  meta = with lib; {
    inherit (pkgs.emacs.meta) platforms;
  };
}
