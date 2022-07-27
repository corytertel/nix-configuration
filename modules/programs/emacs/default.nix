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
      ".emacs.d/themes/smart-mode-line-cory-theme.el".source = ./smart-mode-line-cory-theme.el;
      ".emacs.d/logo.png".source = ./logo.png;
      ".emacs.d/eshell/alias".text = import ./eshell-alias.nix { inherit config pkgs; };
    };

    environment.systemPackages = with pkgs; [
      extract
      githelp
      nixos-test
      nixos-switch
      ripgrep
    ];

  };
}
