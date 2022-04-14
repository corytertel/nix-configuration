{ config, pkgs, ... }:

{
  imports =
    [
      ./apps/bash
      ./apps/firefox
      ./apps/layout_switch
      ./apps/ksh
      ./apps/neofetch
      ./apps/nu
      #./apps/pcmanfm
      ./apps/powershell
      ./apps/ungoogled-chromium
      ./apps/vscode
      ./apps/xfce4-terminal
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
      cmus
      feh
      cmatrix
      bpytop
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
      #wpgtk
      blender
      zathura
      joplin-desktop

      # modern unix
      bat #cat
      exa # ls
      fd # find
      ripgrep # grep
      fzf

      nix-prefetch-github
      git-crypt
      #cool-retro-term
      bb
      #htop
      qbittorrent
      polkit
      libsForQt5.polkit-qt
      brave
      mpv
      pywal
      airshipper
      tree
      nyxt
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
      #virt-manager
      pcmanfm
      #xclip
      powershell
      cinnamon.nemo
      lxappearance
      peek # simple animated gif screen recorder
      sxiv
      leafpad
      onlyoffice-bin
      xfce.orage #simple calendar
      imagemagick
      audacious
      xarchiver
      xorg.xwd
      ledger-live-desktop
      #ledger-udev-rules
      acpi
      xorg.xkill
      gparted
      nushell
      libnotify
      deadd-notification-center
      tint2
      dos2unix
      galculator
      unzip
      klavaro
      obs-studio
      pavucontrol

      #discord
      betterdiscord-installer
      betterdiscordctl
      steamPackages.steamcmd
      minecraft
    ];
  };
}
