{ config, lib, pkgs, ... }:
with lib;

let

  cfg = config.programs.cory.emacs;

  initFile = (builtins.readFile ./init.el)
             + (builtins.readFile ./eshell-undistract-me.el)
             + (import ./eshell-extras.nix { inherit config pkgs; })
             + (if cfg.exwm then builtins.readFile ./exwm.el else "");

  init = pkgs.runCommand "default.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${pkgs.writeText "default.el" initFile} $out/share/emacs/site-lisp/default.el
  '';

  # eshell-undistract-me = pkgs.runCommand "eshell-undistract-me.el" {} ''
  #   mkdir -p $out/share/emacs/site-lisp
  #   cp ${pkgs.writeText "eshell-undistract-me.el" (builtins.readFile ./eshell-undistract-me.el)} $out/share/emacs/site-lisp/eshell-undistract-me.el
  # '';

  emacsPackages = epkgs: with epkgs; [
    init
    use-package
    vterm
    # eshell-undistract-me
  ] ++ (if cfg.exwm then [ epkgs.exwm ] else []);

  emacsPackage = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    alwaysEnsure = true;
    package = pkgs.emacsNativeComp;
    extraEmacsPackages = emacsPackages;
  };

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
      # command = "emacsclient -c";
      command = "emacs";
      # desktopFile = "emacsclient.desktop";
      desktopFile = "emacs.desktop";
      package = emacsPackage;
    };

    home-manager.users.cory.home.file = {
      ".cache/emacs/themes/plain-light-theme.el".source = ./plain-light-theme.el;
      ".cache/emacs/logo.png".source = ./logo.png;
      ".cache/emacs/eshell/alias".text = import ./eshell-alias.nix { inherit config pkgs; };
    };

    environment.systemPackages = with pkgs; [
      extract
      githelp
      nixos-test
      nixos-switch
    ];

  };
}
