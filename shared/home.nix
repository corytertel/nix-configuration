{ config, pkgs, ... }:

{
  imports =
    [
      ./apps/bash
      ./apps/firefox
      ./apps/layout_switch
      ./apps/neofetch
      ./apps/ungoogled-chromium
      ./apps/zsh
    ];

  services = {
    dunst.enable = true;
  };

  programs = {
    home-manager.enable = true;

    direnv.enable = true;
    direnv.nix-direnv.enable = true;

    # eclipse = {
    #   enable = true;
    #   package = pkgs.eclipses.eclipse-java;
    # };

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

  gtk.gtk2.extraConfig = "gtk-key-theme-name = \"Emacs\"";

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
      gwenview
      neofetch
      feh
      btop
      libsForQt5.ark

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
      flameshot
      gimp
      blender
      zathura

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
      polkit
      libsForQt5.polkit-qt
      brave
      mpv
      tree
      xbrightness
      imagemagick
      yt-dlp
      pfetch
      qemu
      qutebrowser
      xscreensaver
      krita
      cava
      tty-clock
      wine64
      winetricks
      grapejuice
      pciutils
      pcmanfm-qt
      lxappearance
      peek # simple animated gif screen recorder
      # leafpad
      # onlyoffice-bin
      xfce.orage
      audacious
      ledger-live-desktop
      #ledger-udev-rules
      acpi
      xorg.xkill
      gparted
      libnotify
      tint2
      dos2unix
      galculator
      unzip
      klavaro
      obs-studio
      pavucontrol
      okular
      vlc
      sxiv
      libreoffice-qt
      pinfo

      wireshark
      tcpdump
      nmap

      betterdiscord-installer
      betterdiscordctl
    ];
  };
}
