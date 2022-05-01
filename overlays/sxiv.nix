{ pkgs, ... }:

final: prev: {
  sxiv = prev.sxiv.overrideAttrs (old: {
    src = pkgs.fetchFromGitHub {
      owner = "corytertel";
      repo = "sxiv";
      rev = "a940d2ace00b7692be7f44574afdfe9e0528bd3d";
      sha256 = "PDJgwqAcjoZcDvFaqC1UBoHp8docaOkZO6FVllZ0VVY=";
    };
  });
}
