{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "sunrise";
  version = "1.0.0";

  commit = "16e6df7e86c7a383fb4400fae94af32baf9cb24e";

  src = pkgs.fetchFromGitHub {
    owner = "sunrise-commander";
    repo = "sunrise-commander";
    rev = "16e6df7e86c7a383fb4400fae94af32baf9cb24e";
    sha256 = "D36qiRi5OTZrBtJ/bD/javAWizZ8NLlC/YP4rdLCSsw=";

  };

  buildInputs = with pkgs; [
    emacs
    emacsPackages.advice
    # emacsPackages.bookmark
    # emacsPackages.cus-edit
    emacsPackages.cl-lib
    # emacsPackages.desktop
    # emacsPackages.dired
    # emacsPackages.dired-aux
    # emacsPackages.dired-x
    # emacsPackages.easymenu
    # emacsPackages.enriched
    # emacsPackages.esh-mode
    # emacsPackages.find-dired
    # emacsPackages.font-lock
    # emacsPackages.hl-line
    # emacsPackages.recentf
    # emacsPackages.sort
    # emacsPackages.term
    # emacsPackages.tramp
    # emacsPackages.tree-widget
  ];

  packageRequires = with pkgs; [
    emacsPackages.advice
    # emacsPackages.bookmark
    # emacsPackages.cus-edit
    emacsPackages.cl-lib
    # emacsPackages.desktop
    # emacsPackages.dired
    # emacsPackages.dired-aux
    # emacsPackages.dired-x
    # emacsPackages.easymenu
    # emacsPackages.enriched
    # emacsPackages.esh-mode
    # emacsPackages.find-dired
    # emacsPackages.font-lock
    # emacsPackages.hl-line
    # emacsPackages.recentf
    # emacsPackages.sort
    # emacsPackages.term
    # emacsPackages.tramp
    # emacsPackages.tree-widget
  ];

  recipe = pkgs.writeText "recipe" ''
    (sunrise
    :repo "sunrise-commander/sunrise-commander"
    :fetcher github
    :files ("*.el"))
  '';

  meta = with lib; {
    inherit (pkgs.emacs.meta) platforms;
  };
}
