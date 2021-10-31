with import <nixpkgs> {};

let inherit (vimUtils) buildVimPluginFrom2Nix; in {

  "coc.nvim" = buildVimPluginFrom2Nix {
    name = "coc.nvim";
    src = fetchgit {

      # github repo
      url = "https://github.com/neoclide/coc.nvim";

      # rev/commit hash
      rev = "5aa3197360a76200e8beb30c9957fac1205b7ad5";

      # sha256 hash
      sha256 = "07ys5mzma5vgiwgy9jzrk03fa93hs3i162l5pg4aqk8wj2dna8g9";
    };
  };

  "Mountain" = buildVimPluginFrom2Nix {
    name = "Mountain";
    src = fetchgit {
      url = "https://github.com/mountain-theme/Mountain/";
      rev = "7e94e07411430900224e208af05b31dd65e06284";
      sha256 = "fmYxbKpfhZslh+ltmmAqqPeiY2fjKYorAszY/PvzuHw=";
    };
  };
}
