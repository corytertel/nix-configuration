{ config, lib, pkgs, ... }:
with lib;

let

  cfg = config.programs.cory.emacs;

  initFile = (builtins.readFile ./elisp/minimal/init-base.el)
             + (builtins.readFile ./elisp/minimal/init-functions.el)
             + (builtins.readFile ./elisp/minimal/init-keybinds.el)

             + (builtins.readFile ./elisp/init-performance.el)
             + (builtins.readFile ./elisp/init-visuals.el)

             # Completion
             + (builtins.readFile ./elisp/init-completion.el)

             # IDE Stuff
             + (builtins.readFile ./elisp/init-lsp.el)
             + (builtins.readFile ./elisp/init-checking.el)
             + (builtins.readFile ./elisp/init-formatting.el)
             + (builtins.readFile ./elisp/init-editing.el)
             + (builtins.readFile ./elisp/init-search.el)
             + (builtins.readFile ./elisp/init-templates.el)
             + (builtins.readFile ./elisp/init-shell.el)
             + (builtins.readFile ./elisp/init-projects.el)
             + (builtins.readFile ./elisp/init-ssh.el)
             + (builtins.readFile ./elisp/init-movement.el)
             + (builtins.readFile ./elisp/init-buffer.el)
             + (builtins.readFile ./elisp/init-help.el)

             # Langs
             + (builtins.readFile ./elisp/init-clojure.el)
             + (builtins.readFile ./elisp/init-common-lisp.el)
             + (builtins.readFile ./elisp/init-cpp.el)
             + (builtins.readFile ./elisp/init-elisp.el)
             + (builtins.readFile ./elisp/init-java.el)
             + (builtins.readFile ./elisp/init-other-langs.el)
             + (builtins.readFile ./elisp/init-python.el)
             + (builtins.readFile ./elisp/init-scheme.el)

             # Other
             + (builtins.readFile ./elisp/init-dired.el)
             + (builtins.readFile ./elisp/init-nixos.el)
             + (builtins.readFile ./elisp/init-org.el)
             + (builtins.readFile ./elisp/init-pdf.el)
             + (builtins.readFile ./elisp/init-eww.el)

             # Window Management
             + (builtins.readFile ./elisp/init-window-management.el)
             + (builtins.readFile ./elisp/aside.el)
             + (builtins.readFile ./elisp/aside-vterm.el)
             + (builtins.readFile ./elisp/aside-eshell.el)
             + (builtins.readFile ./elisp/aside-configurations.el)

             # Informal Packages
             # + (builtins.readFile ./elisp/eshell-undistract-me.el)
             + (builtins.readFile ./elisp/app-launcher.el)

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
      # ".emacs.d/themes/smart-mode-line-cory-theme.el".source = ./smart-mode-line-cory-theme.el;
      ".emacs.d/eshell/alias".source = ./alias;
      ".emacs.d/templates".source = ./templates;
      ".local/share/dict/words".source = "${pkgs.scowl}/share/dict/words.txt";
    };

    environment.systemPackages = let
      tex = (pkgs.texlive.combine {
        inherit (pkgs.texlive) scheme-basic
          dvisvgm dvipng # for preview and export as html
          wrapfig amsmath ulem hyperref capt-of;
      });
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
      # dirvish utilities
      fd
      imagemagick
      poppler
      ffmpegthumbnailer
      mediainfo
      # ssh
      sshfs
      fuse
      # dictionary
      scowl
      ispell
      # for emacs-everywhere
      # xclip
      # xdotool
      # xorg.xprop
      # xorg.xwininfo
      # fish
    ] ++ shellScripts;

  };
}
