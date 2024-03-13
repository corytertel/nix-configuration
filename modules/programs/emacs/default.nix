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

  +
  ''
  (defvar monospace-font-name "${config.programs.cory.emacs.fonts.monospace.name}")
  (defvar monospace-font-height ${toString config.programs.cory.emacs.fonts.monospace.size})
  (defvar variable-font-name "${config.programs.cory.emacs.fonts.variable.name}")
  (defvar variable-font-height ${toString config.programs.cory.emacs.fonts.variable.size})
  ''
  + (builtins.readFile ./elisp/init-visuals.el)

  # Completion
  + ''(add-to-list 'load-path "${(pkgs.callPackage ./hotfuzz-module.nix
    { inherit pkgs; emacs = config.programs.cory.emacs.package; })}")''
  + (builtins.readFile ./elisp/vertico-frame.el)
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
  + (builtins.readFile ./elisp/init-elisp.el)
  # + (builtins.readFile ./elisp/init-clojure.el)
  + (builtins.readFile ./elisp/init-common-lisp.el)
  + (builtins.readFile ./elisp/init-cpp.el)
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
  + (if config.programs.cory.emacs.popup
     then builtins.readFile ./elisp/init-popup.el
     else # ''(add-to-list 'load-path "${(pkgs.callPackage ./eaf.nix { inherit pkgs; })}")''
       builtins.readFile ./elisp/init-no-popup.el)

  # Informal Packages
  # + (builtins.readFile ./elisp/eshell-undistract-me.el)
  + (builtins.readFile ./elisp/app-launcher.el)

  # Repeat Maps (last)
  + (builtins.readFile ./elisp/init-repeat-maps.el)

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
      # tree-sitter-clojure
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
      EDITOR = "emacsclient -nw";
      VISUAL = "emacsclient -c -a ''";
      # eaf
      QT_QPA_PLATFORM_PLUGIN_PATH = "${pkgs.qt6.qtbase.outPath}/lib/qt-6/plugins";
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

      # eaf
      git
      nodejs
      # Causes weird cursor bugs on xmonad that make emacs unusable
      # wmctrl
      # xdotool
      ((pkgs.python311.withPackages(ps: with ps; [
        pandas
        requests
        sexpdata tld
        pyqt6 pyqt6-sip
        pyqt6-webengine epc lxml # for eaf
        qrcode # eaf-file-browser
        pysocks # eaf-browser
        pymupdf # eaf-pdf-viewer
        pypinyin # eaf-file-manager
        psutil # eaf-system-monitor
        retry # eaf-markdown-previewer
        markdown
      ])).override { ignoreCollisions = true; })
      # eaf-browser
      aria
      # eaf-file-manager
      fd
    ];

  };
}
