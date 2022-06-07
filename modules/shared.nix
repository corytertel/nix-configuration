{ config, pkgs, ... }:

{
  programs = {
    home-manager.enable = true;

    direnv.enable = true;
    direnv.nix-direnv.enable = true;

    java = {
      enable = true;
      package = pkgs.jdk;
    };

    git = {
      enable = true;
      userName  = "corytertel";
      userEmail = "ctertel@comcast.net";
    };
  };

  home = {
    username = "cory";
    homeDirectory = "/home/cory";

    packages = let
      tex = (pkgs.texlive.combine {
        inherit (pkgs.texlive) scheme-basic
          dvisvgm dvipng # for preview and export as html
          wrapfig amsmath ulem hyperref capt-of;
        #(setq org-latex-compiler "lualatex")
        #(setq org-preview-latex-default-process 'dvisvgm)
      });
    in with pkgs; [
      # linux basics
      killall
      btop
      lxqt.lxqt-archiver

      # development basics
      ccls
      clang_12
      #llvmPackages_12.clang-unwrapped # clangd
      clang-tools # clang-tidy and clang-analyzer
      #llvmPackages_13.libclang
      #libclang
      llvmPackages_12.libcxx
      #libcxx
      #clang-analyzer
      cppcheck
      ncurses
      global
      gdb
      nodejs
      yarn
      tex

      # clojure
      clisp
      clojure
      #clojure-lsp
      leiningen
      joker
      clj-kondo

      # other programing languages
      python39Full
      python39Packages.pip
      rustc
      rust-analyzer
      racket

      # essential user apps
      tdesktop
      photogimp
      blender
      # epdfview

      # modern unix
      bat #cat
      exa # ls
      fd # find
      ripgrep # grep
      fzf

      nix-prefetch-github
      git-crypt
      bb
      qbittorrent
      brave
      tree
      imagemagick
      yt-dlp
      qemu
      qutebrowser
      wine64
      winetricks
      grapejuice
      pciutils
      pcmanfm-qt
      peek # simple animated gif screen recorder
      # leafpad
      # onlyoffice-bin
      audacious
      ledger-live-desktop
      #ledger-udev-rules
      acpi
      gparted
      libnotify
      dos2unix
      galculator
      unzip
      klavaro
      obs-studio
      # okular
      vlc
      libreoffice-qt
      pinfo
      fd
      lxqt.lximage-qt

      wireshark
      tcpdump
      nmap
    ];
  };
}
