{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "eglot-booster";
  version = "20240818";

  commit = "e19dd7ea81bada84c66e8bdd121408d9c0761fe6";

  src = pkgs.fetchFromGitHub {
    owner = "jdtsmith";
    repo = "eglot-booster";
    rev = "e19dd7ea81bada84c66e8bdd121408d9c0761fe6";
    hash = "sha256-vF34ZoUUj8RENyH9OeKGSPk34G6KXZhEZozQKEcRNhs=";
  };

  buildInputs = with pkgs; [
    emacs
    emacsPackages.eglot
    emacsPackages.jsonrpc
  ];

  packageRequires = with pkgs; [
    emacsPackages.eglot
    emacsPackages.jsonrpc
  ];

  recipe = pkgs.writeText "recipe" ''
    (eglot-booster
    :repo "jdtsmith/eglot-booster"
    :fetcher github
    :files ("*.el"))
  '';

  meta = with lib; {
    inherit (pkgs.emacs.meta) platforms;
  };
}
