{ config, lib, pkgs }:

pkgs.emacsPackages.melpaBuild rec {
  pname = "macrursors";
  version = "1.0.0";

  commit = "5c0a5052118b8dd2c1787dba9b06cb0a1514c268";

  src = pkgs.fetchFromGitHub {
    owner = "corytertel";
    repo = "macrursors";
    rev = "bd7b9e61228d14b3d0335f061b522458dcd1769a";
    sha256 = "UoPv+SnIoBzv5r/wlrnSwfJfR8Vlv/UiWXuMje1e5YU=";
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
