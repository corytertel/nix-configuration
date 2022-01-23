{ pkgs, ... }:

{
  home.file.".emacs.d" = {
    # don't make the directory read only so that impure melpa can still happen
    # for now
    recursive = true;
    source = pkgs.fetchFromGitHub {
      owner = "syl20bnr";
      repo = "spacemacs";
      rev = "2a668ab90d92421dc5c14bb6440d1bb1a1f99e9a";
      sha256 = "9ez5bkEMBy+Z2LVFmyTBHVvL+Py4QbAW+UfIx9ILyrE=";
    };
  };

  home.file = {
    ".spacemacs".text = builtins.readFile ./spacemacs;
    ".emacs.d/core/banners/img/emacs-logo.png".source = ./emacs-logo.png;
  };
}
