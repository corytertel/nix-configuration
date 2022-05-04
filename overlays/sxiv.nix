{ pkgs, ... }:

final: prev: {
  sxiv = prev.sxiv.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "sxiv";
      rev = "ba6f3fd45d0741070d9b74319ed5fb606cc79dfe";
      sha256 = "PDJgwqAcjoZcDvFaqC1UBoHp8docaOkZO6FVllZ0VVY=";
    };
  });
}
