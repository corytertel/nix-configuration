{ config, lib, pkgs, ... }:
with lib;

let

  cfg = config.programs.cory.emacs;

  initFile = '';;; default.el --- init -*- lexical-binding: t; no-byte-compile: nil; -*-
  ''
  + (builtins.readFile ./elisp/init-performance.el)
  + (builtins.readFile ./elisp/init-base.el)
  + (builtins.readFile ./elisp/init-functions.el)
  + (builtins.readFile ./elisp/init-keybinds.el)
  + (builtins.readFile ./elisp/init-search.el)
  + (builtins.readFile ./elisp/init-eww.el)

  + (builtins.readFile ./elisp/init-help.el)
  + (builtins.readFile ./elisp/init-visuals.el)

  # Completion
  + ''(add-to-list 'load-path "${(pkgs.callPackage ./hotfuzz-module.nix
    { inherit pkgs; emacs = emacsBasePackage; })}")''
  + (builtins.readFile ./elisp/init-completion.el)
  + (builtins.readFile ./elisp/init-lsp.el)
  + (builtins.readFile ./elisp/init-snippets.el)
  + (builtins.readFile ./elisp/company-yasnippet.el)

  # IDE Stuff
  + (builtins.readFile ./elisp/init-checking.el)
  + (builtins.readFile ./elisp/init-formatting.el)
  + (builtins.readFile ./elisp/init-editing.el)
  + (builtins.readFile ./elisp/init-shell.el)
  + (builtins.readFile ./elisp/init-projects.el)
  + (builtins.readFile ./elisp/init-ssh.el)
  + (builtins.readFile ./elisp/init-movement.el)
  + (builtins.readFile ./elisp/init-buffer.el)

  # Langs
  + (builtins.readFile ./elisp/init-clojure.el)
  + (builtins.readFile ./elisp/init-common-lisp.el)
  + (builtins.readFile ./elisp/init-cpp.el)
  + (builtins.readFile ./elisp/init-elisp.el)
  + (builtins.readFile ./elisp/init-java.el)
  + (builtins.readFile ./elisp/init-apl.el)
  + (builtins.readFile ./elisp/init-other-langs.el)
  + (builtins.readFile ./elisp/init-python.el)
  + (builtins.readFile ./elisp/init-scheme.el)
  + (builtins.readFile ./elisp/init-web.el)

  # Other
  + (builtins.readFile ./elisp/init-dired.el)
  + (builtins.readFile ./elisp/init-nixos.el)
  + (builtins.readFile ./elisp/init-org.el)

  # Window Management
  + (builtins.readFile ./elisp/init-window-management.el)

  # Informal Packages
  # + (builtins.readFile ./elisp/eshell-undistract-me.el)
  + (builtins.readFile ./elisp/app-launcher.el)

  # Repeat Maps (last)
  + (builtins.readFile ./elisp/init-repeat-maps.el);

  init = pkgs.runCommand "default.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${pkgs.writeText "default.el" initFile} $out/share/emacs/site-lisp/default.el
  '';

  emacsBasePackage = pkgs.emacs-git.override {
    withGTK3 = true;
    withTreeSitter = true;
  };

  emacsPackages = epkgs: with epkgs; [
    init
    use-package
    vterm
    (treesit-grammars.with-grammars (p: with p; [
      tree-sitter-c
      tree-sitter-cpp
      tree-sitter-bash
      tree-sitter-java
      tree-sitter-javascript
      tree-sitter-html
      tree-sitter-css
      tree-sitter-haskell
      tree-sitter-ocaml
      tree-sitter-elisp
      tree-sitter-nix
      tree-sitter-nu
      # don't need these three rn because of scope highlighting
      # tree-sitter-scheme
      # tree-sitter-commonlisp
      # tree-sitter-clojure
    ]))
  ];

  emacsPackage = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    alwaysEnsure = true;
    package = emacsBasePackage;
    extraEmacsPackages = emacsPackages;
    override = epkgs: epkgs // {
      macrursors = pkgs.callPackage ./macrursors.nix {};
      cape-yasnippet = pkgs.callPackage ./cape-yasnippet.nix {};
    };
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
      command = "emacsclient -c -e '(fancy-startup-screen)'";
      desktopFile = "emacsclient.desktop";
      package = emacsPackage;
    };

    home-manager.users.cory.home.file = {
      ".emacs.d/eshell/alias".source = ./alias;
      ".emacs.d/snippets" = {
        source = ./snippets;
        recursive = true;
      };
      ".local/share/dict/words".source = "${pkgs.scowl}/share/dict/words.txt";
    };

    environment.systemPackages = let
      # tex = (pkgs.texlive.combine {
      #   inherit (pkgs.texlive)
      #     # scheme-basic
      #     scheme-small
      #     dvisvgm
      #     dvipng
      #     wrapfig
      #     amsmath
      #     ulem
      #     hyperref
      #     capt-of;
      # });
      tex = pkgs.texlive.combined.scheme-full;
    in with pkgs; [
      tex
      git
      ripgrep
      flameshot
      # eshell-undistract-me
      sound-theme-freedesktop
      pulseaudio
      libnotify
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
      # ssh
      sshfs
      fuse
      # dictionary
      scowl
      ispell
      # for emacs-everywhere
      xclip
      xdotool
      xorg.xprop
      xorg.xwininfo
      # fish
      # spelling
      enchant
      pkgconf
      aspell
      aspellDicts.en
      hunspell
      hunspellDicts.ru_RU
    ];

  };
}
