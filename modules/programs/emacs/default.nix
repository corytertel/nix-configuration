{ config, lib, pkgs, ... }:
with lib;

let

  cfg = config.programs.cory.emacs;

  initFile = '';;; default.el --- init -*- lexical-binding: t; no-byte-compile: nil; -*-
  ''
  + (builtins.readFile ../../../config/emacs/elisp/init-performance.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-base.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-functions.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-keybinds.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-search.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-eww.el)

  + (builtins.readFile ../../../config/emacs/elisp/init-help.el)

  +
  ''
  (defvar monospace-font-name "${config.programs.cory.emacs.fonts.monospace.name}")
  (defvar monospace-font-height ${toString config.programs.cory.emacs.fonts.monospace.size})
  (defvar variable-font-name "${config.programs.cory.emacs.fonts.variable.name}")
  (defvar variable-font-height ${toString config.programs.cory.emacs.fonts.variable.size})
  ''
  + (builtins.readFile ../../../config/emacs/elisp/init-visuals.el)

  # Completion
  + ''(add-to-list 'load-path "${(pkgs.callPackage ./hotfuzz-module.nix
    { inherit pkgs; emacs = config.programs.cory.emacs.package; })}")''
  + (builtins.readFile ../../../config/emacs/elisp/vertico-frame.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-completion.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-lsp.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-treesitter.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-snippets.el)
  + (builtins.readFile ../../../config/emacs/elisp/company-yasnippet.el)

  # IDE Stuff
  + (builtins.readFile ../../../config/emacs/elisp/init-checking.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-formatting.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-editing.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-shell.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-projects.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-ssh.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-movement.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-buffer.el)

  # Langs
  + (builtins.readFile ../../../config/emacs/elisp/init-elisp.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-common-lisp.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-cpp.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-java.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-apl.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-other-langs.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-python.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-scheme.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-web.el)

  # Other
  + (builtins.readFile ../../../config/emacs/elisp/init-dired.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-nixos.el)
  + (builtins.readFile ../../../config/emacs/elisp/init-org.el)
  + (if config.programs.cory.emacs.eaf
     then ''(add-to-list 'load-path "${(pkgs.callPackage ./eaf.nix { inherit pkgs; })}")''
          + (builtins.readFile ../../../config/emacs/elisp/init-eaf.el)
     else "")

  # Window Management
  + (builtins.readFile ../../../config/emacs/elisp/init-window-management.el)
  + (if config.programs.cory.emacs.popup
     then builtins.readFile ../../../config/emacs/elisp/init-popup.el
     else ''''
          + builtins.readFile ../../../config/emacs/elisp/init-no-popup.el)

  # Informal Packages
  + (builtins.readFile ../../../config/emacs/elisp/eshell-undistract-me.el)
  + (builtins.readFile ../../../config/emacs/elisp/app-launcher.el)

  # Repeat Maps (last)
  # + (builtins.readFile ../../../config/emacs/elisp/init-repeat-maps.el)

  + config.programs.cory.emacs.extraConfig;

  init = pkgs.runCommand "default.el" {} ''
    mkdir -p $out/share/emacs/site-lisp
    cp ${pkgs.writeText "default.el" initFile} $out/share/emacs/site-lisp/default.el
  '';

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
      tree-sitter-python
      # don't need these three rn because of scope highlighting
      # tree-sitter-scheme
      # tree-sitter-commonlisp
    ]))
  ];

  emacsPackage = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    alwaysEnsure = true;
    package = config.programs.cory.emacs.package;
    extraEmacsPackages = emacsPackages;
    override = epkgs: epkgs // {
      macrursors = pkgs.callPackage ./macrursors.nix {};
      cape-yasnippet = pkgs.callPackage ./cape-yasnippet.nix {};
      combobulate = pkgs.callPackage ./combobulate.nix {};
      evil-motion-trainer = pkgs.callPackage ./evil-motion-trainer.nix {};
    };
  };

in {

  options.programs.cory.emacs = {
    enable = mkEnableOption "Enables emacs";
    package = mkOption {
      type = types.package;
      default = pkgs.emacs-git.override {
        withGTK3 = true;
        withTreeSitter = true;
        withNativeCompilation = true;
      };
    };
    popup = mkOption {
      type = types.bool;
      default = false;
    };
    eaf = mkOption {
      type = types.bool;
      default = !config.programs.cory.emacs.popup;
    };
    fonts = {
      monospace = {
        package = mkOption {
          type = types.package;
          default = config.theme.font.monospace.package;
        };
        name = mkOption {
          type = types.str;
          default = config.theme.font.monospace.name;
        };
        size = mkOption {
          type = types.int;
          default = 100;
        };
      };
      variable = {
        package = mkOption {
          type = types.package;
          default = config.theme.font.serif.package;
        };
        name = mkOption {
          type = types.str;
          default = config.theme.font.serif.name;
        };
        size = mkOption {
          type = types.int;
          default = 100;
        };
      };
    };
    extraConfig = mkOption {
      type = types.str;
      default = "";
    };
  };

  config = mkIf cfg.enable {

    fonts.packages = [
      config.programs.cory.emacs.fonts.monospace.package
      config.programs.cory.emacs.fonts.variable.package
    ];

    environment.variables = {
      ALTERNATE_EDITOR = "emacs -nw";
      # emacsclient does not work bc of eaf
      # EDITOR = "emacsclient -nw";
      # VISUAL = "emacsclient -c -a ''";
      EDITOR = "emacs -nw";
      VISUAL = "emacs -c -a ''";
      # eaf
      QT_QPA_PLATFORM_PLUGIN_PATH = "${pkgs.qt6.qtbase.outPath}/lib/qt-6/plugins";
    };

    apps.editor = {
      name = "emacs";
      # command = "emacsclient -c";
      # desktopFile = "emacsclient.desktop";
      # emacsclient does not work bc of eaf
      command = "emacs -c";
      desktopFile = "emacs.desktop";
      package = emacsPackage;
    };

    home-manager.users.cory.home.file = {
      ".emacs.d/eshell/alias".source = ../../../config/emacs/alias;
      ".emacs.d/snippets" = {
        source = ../../../config/emacs/snippets;
        recursive = true;
      };
      ".local/share/dict/words".source = "${pkgs.scowl}/share/dict/words.txt";
    };

    environment.systemPackages = let
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
