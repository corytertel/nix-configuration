{ config, lib, pkgs, ... }:
with lib;

let

  cfg = config.programs.cory.emacs;

  initFile = (builtins.readFile ./init.el);

  init = pkgs.runCommand "default.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${pkgs.writeText "default.el" initFile} $out/share/emacs/site-lisp/default.el
  '';

  emacsPackages = epkgs: with epkgs; [
    init
    use-package
    vterm
  ];

  emacsPackage = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    alwaysEnsure = true;
    package = pkgs.emacsNativeComp;
    extraEmacsPackages = emacsPackages;
  };

in {

  options.programs.cory.emacs = {
    enable = mkEnableOption "Enables emacs";
  };

  config = mkIf cfg.enable {

    environment.variables = {
      ALTERNATE_EDITOR = "emacs -nw";
      EDITOR = "emacsclient -nw";
      VISUAL = "emacsclient -c -a ''";
    };

    apps.editor = {
      name = "emacs";
      command = "emacsclient -c";
      desktopFile = "emacsclient.desktop";
      package = emacsPackage;
    };

    home-manager.users.cory.home.file = {
      ".cache/emacs/themes/plain-light-theme.el".source = ./plain-light-theme.el;
      ".cache/emacs/logo.png".source = ./logo.png;
    };

  };
}
