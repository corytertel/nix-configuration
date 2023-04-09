{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "macrursors";
  version = "1.0.0";

  commit = "9c1da4ddf32018ead96056622388873550429ce1";

  src = pkgs.fetchFromGitHub {
    owner = "corytertel";
    repo = "macrursors";
    rev = "9c1da4ddf32018ead96056622388873550429ce1";
    sha256 = "Ws2OQoZ85IoFPqeDHwArEy7JQPBruCKaXb1fDVhE/9A=";
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
