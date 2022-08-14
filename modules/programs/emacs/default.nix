{ config, lib, pkgs, ... }:
with lib;

let

  cfg = config.programs.cory.emacs;

  initFile = (builtins.readFile ./init.el)
             + (builtins.readFile ./aweshell.el)
             + (builtins.readFile ./eshell-undistract-me.el)
             + (import ./eshell-extras.nix { inherit config pkgs; })
             + (if cfg.exwm then builtins.readFile ./exwm.el else "");

  init = pkgs.runCommand "default.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${pkgs.writeText "default.el" initFile} $out/share/emacs/site-lisp/default.el
  '';

  emacsPackages = epkgs: with epkgs; [
    init
    use-package
    vterm
  ] ++ (if cfg.exwm then [ epkgs.exwm ] else []);

  emacsPackage = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    alwaysEnsure = true;
    package = pkgs.emacsNativeComp;
    extraEmacsPackages = emacsPackages;
  };

  shellScripts = [
    # nixos-test
    (pkgs.writeShellScriptBin "nixos-test" ''
      nixos-rebuild test --flake .#$1 --use-remote-sudo
    '')

    # nixos-switch
    (pkgs.writeShellScriptBin "nixos-switch" ''
      nixos-rebuild switch --flake .#$1 --use-remote-sudo
    '')

    # extract
    (pkgs.writeShellScriptBin "extract" ''
      if [ -f $1 ] ; then
          case $1 in
              *.tar.bz2) tar xjf $1 ;;
              *.tar.gz) tar xzf $1 ;;
              *.bz2) bunzip2 $1 ;;
              *.rar) rar x $1 ;;
              *.gz) gunzip $1 ;;
              *.tar) tar xf $1 ;;
              *.tbz2) tar xjf $1 ;;
              *.tgz) tar xzf $1 ;;
              *.zip) unzip $1 ;;
              *.Z) uncompress $1 ;;
              *) echo "'$1' cannot be extracted via extract()" ;;
          esac
      else
          echo "'$1' is not a valid file"
      fi
    '')

    # githelp
    (pkgs.writeShellScriptBin "githelp" ''
      echo "-------------------------------------------------------------------------------"
      echo "git clone http://... [repo-name]"
      echo "git init [repo-name]"
      echo "-------------------------------------------------------------------------------"
      echo "git add -A <==> git add . ; git add -u # Add to the staging area (index)"
      echo "-------------------------------------------------------------------------------"
      echo "git commit -m 'message' -a"
      echo "git commit -m 'message' -a --amend"
      echo "-------------------------------------------------------------------------------"
      echo "git status"
      echo "git log --stat # Last commits, --stat optional"
      echo "git ls-files"
      echo "git diff HEAD~1..HEAD"
      echo "-------------------------------------------------------------------------------"
      echo "git push origin master"
      echo "git push origin master:master"
      echo "-------------------------------------------------------------------------------"
      echo "git remote add origin http://..."
      echo "git remote set-url origin git://..."
      echo "-------------------------------------------------------------------------------"
      echo "git stash"
      echo "git pull origin master"
      echo "git stash list ; git stash pop"
      echo "-------------------------------------------------------------------------------"
      echo "git submodule add /absolute/path repo-name"
      echo "git submodule add http://... repo-name"
      echo "-------------------------------------------------------------------------------"
      echo "git checkout -b new-branch <==> git branch new-branch ; git checkout new-branch"
      echo "git merge old-branch"
      echo "git branch local_name origin/remote_name # Associate branches"
      echo "-------------------------------------------------------------------------------"
      echo "git update-index --assume-unchanged <file> # Ignore changes"
      echo "git rm --cached <file> # Untrack a file"
      echo "-------------------------------------------------------------------------------"
      echo "git reset --hard HEAD # Repair what has been done since last commit"
      echo "git revert HEAD # Repair last commit"
      echo "git checkout [file] # Reset a file to its previous state at last commit"
      echo "-------------------------------------------------------------------------------"
      echo "git tag # List"
      echo "git tag v0.5 # Lightwieght tag"
      echo "git tag -a v1.4 -m 'my version 1.4' # Annotated tag"
      echo "git push origin v1.4 # Pushing"
      echo "-------------------------------------------------------------------------------"
      echo "HOW TO RENAME A BRANCH LOCALLY AND REMOTELY"
      echo "git branch -m old_name new_name"
      echo "git push origin new_name"
      echo "git push origin :old_name"
      echo "------"
      echo "Each other client of the repository has to do:"
      echo "git fetch origin ; git remote prune origin"
      echo "-------------------------------------------------------------------------------"
    '')
  ];

in {

  options.programs.cory.emacs = {
    enable = mkEnableOption "Enables emacs";
    exwm = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {

    environment.variables = {
      ALTERNATE_EDITOR = "emacs -nw";
      EDITOR = "emacsclient -nw";
      VISUAL = "emacsclient -c -a ''";
    };

    apps.editor = {
      name = "emacs";
      command = "emacsclient -c -e '(fancy-startup-screen)'";
      desktopFile = "emacsclient.desktop";
      package = emacsPackage;
    };

    home-manager.users.cory.home.file = {
      ".emacs.d/themes/plain-light-theme.el".source = ./plain-light-theme.el;
      ".emacs.d/themes/plain-grey-theme.el".source = ./plain-grey-theme.el;
      ".emacs.d/themes/smart-mode-line-cory-theme.el".source = ./smart-mode-line-cory-theme.el;
      ".emacs.d/logo.png".source = ./logo.png;
      ".emacs.d/eshell/alias".text = import ./eshell-alias.nix { inherit config pkgs; };
    };

    environment.systemPackages = with pkgs; [
      git
      ripgrep
    ] ++ shellScripts;

  };
}
