{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "combobulate";
  version = "20240315";

  commit = "ee82c568ad639605518f62f82fae4bcc0dfdbb81";

  src = pkgs.fetchFromGitHub {
    owner = "mickeynp";
    repo = "combobulate";
    rev = "ee82c568ad639605518f62f82fae4bcc0dfdbb81";
    hash = "sha256-rww0/6304xZWTFRo1BVcfSDdXOXtlgmfZOxAoOIjYsk=";
  };

  buildInputs = with pkgs; [
    emacs
    emacsPackages.cl-lib
    emacsPackages.map
    emacsPackages.multiple-cursors
    emacsPackages.python
    emacsPackages.transient
    emacsPackages.xref
  ];

  packageRequires = with pkgs; [
    emacsPackages.cl-lib
    emacsPackages.map
    emacsPackages.multiple-cursors
    emacsPackages.python
    emacsPackages.transient
    emacsPackages.xref
  ];

  recipe = pkgs.writeText "recipe" ''
    (combobulate
    :repo "mickeynp/combobulate"
    :fetcher github
    :files ("*.el"))
  '';

  meta = with lib; {
    inherit (pkgs.emacs.meta) platforms;
  };
}
