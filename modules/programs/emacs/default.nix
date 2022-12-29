{ config, lib, pkgs, ... }:
with lib;

let

  cfg = config.programs.cory.emacs;

  initFile = (builtins.readFile ./init.el)
             # + (builtins.readFile ./aweshell.el)
             + (builtins.readFile ./eshell-undistract-me.el)
             + (builtins.readFile ./app-launcher.el)
             + (builtins.readFile ./aside.el)
             + (builtins.readFile ./aside-vterm.el)
             + (builtins.readFile ./aside-eshell.el)
             + (builtins.readFile ./aside-configurations.el)
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
    package = pkgs.emacsGit;
    extraEmacsPackages = emacsPackages;
    override = epkgs: epkgs // {
      sunrise = pkgs.callPackage ./sunrise-commander.nix {};
      macrursors = pkgs.callPackage ./macrursors.nix {};
    };
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
      # ".emacs.d/themes/plain-grey-theme.el".source = ./plain-grey-theme.el;
      # ".emacs.d/themes/plain-dark-theme.el".source = ./plain-dark-theme.el;
      # ".emacs.d/themes/plain-summer-theme.el".source = ./plain-summer-theme.el;
      # ".emacs.d/themes/plain-ocean-theme.el".source = ./plain-ocean-theme.el;
      # ".emacs.d/themes/smart-mode-line-cory-theme.el".source = ./smart-mode-line-cory-theme.el;
      ".emacs.d/eshell/alias".text = import ./eshell-alias.nix { inherit config pkgs; };
      ".emacs.d/templates".source = ./templates;
      # ".emacs.d/snippets".source = ./snippets;
      ".local/share/dict/words".source = "${pkgs.scowl}/share/dict/words.txt";
    };

    environment.systemPackages = with pkgs; [
      git
      ripgrep
      flameshot
      # eshell-undistract-me
      sound-theme-freedesktop
      # dired archive utilities
      avfs
      gnutar
      gzip
      bzip2
      xz
      zip
      unzip
      rar
      rpm
      dpkg
      arj
      lha
      p7zip
      # dirvish utilities
      fd
      imagemagick
      poppler
      ffmpegthumbnailer
      mediainfo
      # dictionary
      scowl
      ispell
      # for emacs-everywhere
      xclip
      xdotool
      xorg.xprop
      xorg.xwininfo
      fish
    ] ++ shellScripts;

  };
}
