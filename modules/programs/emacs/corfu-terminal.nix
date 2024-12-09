{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "corfu-terminal";
  version = "0.7";

  commit = "501548c3d51f926c687e8cd838c5865ec45d03cc";

  src = builtins.fetchGit {
    url = "https://codeberg.org/akib/emacs-corfu-terminal.git";
    ref = "master";
    rev = "501548c3d51f926c687e8cd838c5865ec45d03cc";
  };

  buildInputs = with pkgs; [
    emacs
    emacsPackages.cl-lib
    emacsPackages.subr-x
    emacsPackages.corfu
    emacsPackages.popon
  ];

  packageRequires = with pkgs; [
    emacsPackages.cl-lib
    emacsPackages.subr-x
    emacsPackages.corfu
    emacsPackages.popon
  ];

  recipe = pkgs.writeText "recipe" ''
    (corfu-terminal
    :repo "akib/corfu-terminal"
    :fetcher codeberg
    :files ("*.el"))
  '';

  meta = with lib; {
    inherit (pkgs.emacs.meta) platforms;
  };
}
