with import <nixpkgs> {};

let inherit (vimUtils) buildVimPluginFrom2Nix; in {
  #"Mountain" = buildVimPluginFrom2Nix {
  #  name = "Mountain";
  #  src = fetchgit {
  #    url = "https://github.com/mountain-theme/Mountain/";
  #    rev = "7e94e07411430900224e208af05b31dd65e06284";
  #    sha256 = "fmYxbKpfhZslh+ltmmAqqPeiY2fjKYorAszY/PvzuHw=";
  #  };
  #};
}
