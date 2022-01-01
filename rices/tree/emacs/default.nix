{ pkgs, ... }:

{
  home.file.".emacs.d" = {
    # don't make the directory read only so that impure melpa can still happen
    # for now
    recursive = true;
    source = pkgs.fetchFromGitHub {
      owner = "syl20bnr";
      repo = "spacemacs";
      rev = "9f92b47ce63c32a074471041f35b9383c3488e3d";
      sha256 = "4upBcpg0lh+VyLD9rZEoo3rEEYtBzKoojILRYFwgjn0=";
    };
  };

  home.file = {
    ".spacemacs".text = builtins.readFile ./spacemacs;
    ".emacs.d/core/banners/img/emacs-logo-mountain.png".source = ./emacs-logo-mountain.png;
  };
}
