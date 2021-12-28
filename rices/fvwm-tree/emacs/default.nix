{ pkgs, ... }:

{
  home.file.".emacs.d" = {
    # don't make the directory read only so that impure melpa can still happen
    # for now
    recursive = true;
    source = pkgs.fetchFromGitHub {
      owner = "syl20bnr";
      repo = "spacemacs";
      rev = "4f7246da07e7eb1e07a26de23a80cd2d89a89a7c";
      sha256 = "4DjCFQigEU6OcsAn/6vjUYO01KLilI3wqhS/Xn/UEH8=";
    };
  };

  home.file = {
    ".spacemacs".text = builtins.readFile ./spacemacs;
    ".emacs.d/core/banners/img/emacs-logo-mountain.png".source = ./emacs-logo-mountain.png;
  };
}
