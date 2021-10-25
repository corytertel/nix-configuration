{ pkgs, ... }:

{
  home.file.".emacs.d" = {
    # don't make the directory read only so that impure melpa can still happen
    # for now
    recursive = true;
    source = pkgs.fetchFromGitHub {
      owner = "syl20bnr";
      repo = "spacemacs";
      rev = "63056ecb50f93808781b97feab1c3225d35c7aa1";
      sha256 = "/bxfNUcMq1Wcj1RAYi82LNk67nKAFjjQ/osA0rx1GQ0=";
    };
  };

  home.file = {
    ".spacemacs".text = builtins.readFile ./spacemacs;
    ".emacs.d/core/banners/005-banner.txt".text = builtins.readFile ./005-banner.txt;
    ".emacs.d/core/banners/006-banner.txt".text = builtins.readFile ./006-banner.txt;
    ".emacs.d/core/banners/007-banner.txt".text = builtins.readFile ./007-banner.txt;
    ".emacs.d/private/themes/mountain-theme/mountain.el".text = builtins.readFile ./mountain.el;
  };
}
