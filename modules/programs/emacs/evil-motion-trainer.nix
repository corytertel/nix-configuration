{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "evil-motion-trainer";
  version = "20210807";

  commit = "32472acace6be6d99af1ab16cecaaea4344471ec";

  src = pkgs.fetchFromGitHub {
    owner = "martinbaillie";
    repo = "evil-motion-trainer";
    rev = "32472acace6be6d99af1ab16cecaaea4344471ec";
    hash = "sha256-FsbSS+WdtjqjDIYRY0f/45OTVDFuZRswp29yfw/1O58=";
  };

  buildInputs = with pkgs; [
    emacs
    emacsPackages.cl-lib
    emacsPackages.evil
  ];

  packageRequires = with pkgs; [
    emacsPackages.cl-lib
    emacsPackages.evil
  ];

  recipe = pkgs.writeText "recipe" ''
    (evil-motion-trainer
    :repo "martinbaillie/evil-motion-trainer"
    :fetcher github
    :files ("*.el"))
  '';

  meta = with lib; {
    inherit (pkgs.emacs.meta) platforms;
  };
}
