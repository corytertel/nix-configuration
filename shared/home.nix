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
    stateVersion = "21.11";
    packages = with pkgs; [
      # linux basics
      killall
      kitty
      gwenview
      neofetch
      cmus
      feh
      cmatrix
      dmenu
      networkmanager_dmenu
      bpytop
      atool
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

      # clojure
      clisp
      clojure
      clojure-lsp
      leiningen
      joker
      clj-kondo

      # other programing languages
      python39Full
      python39Packages.pip
      rustc
      rust-analyzer

      # essential user apps
      tdesktop
      flameshot
      libreoffice
      gimp
      #wpgtk
      blender
      zathura
      joplin-desktop

      # modern unix
      bat #cat
      exa # ls
      #lsd # ls
      #delta # viewer for git and diff output
      #dust # du
      #duf # df
      #broot # tree
      fd # find
      ripgrep # grep
      #ag # ack
      fzf
      #mcfly
      #choose # cut and sometimes awk
      #jq # sed for json
      #sd # sed
      #cheat # cheatsheet
      #tldr # man
      #hyperfine
      #gping
      #procs # ps
      #httpie
      #curlie # curl
      #xh
      #zoxide # cd
      #dog # dig

      # other essentials
      #nerdfonts
      #roboto-mono
      #libsForQt5.krohnkite
      #libsForQt5.plasma-applet-virtual-desktop-bar
      nix-prefetch-github
      #haskellPackages.xmobar
      #latte-dock
      #libsForQt5.qtstyleplugin-kvantum
      #conky
      git-crypt
      gnupg
      #cool-retro-term
      bb
      #htop
      qbittorrent
      audacity
      polkit
      libsForQt5.polkit-qt
      brave
      celluloid
      #gnome.gnome-disk-utility
      mpv
      pywal
      airshipper
      tree
      #gnome.nautilus
      #nyxt
      xbrightness
      ncmpcpp
      imagemagick
      yt-dlp
      libsForQt5.kcalc
      pfetch
      qemu
      qutebrowser
      #xfce.thunar
      xscreensaver
      krita
      cava
      tty-clock
      wine64
      winetricks
      grapejuice
      pciutils
      virt-manager
      pcmanfm
      #xclip
      powershell
      cinnamon.nemo
      #krusader
      lxappearance
      libsForQt5.dolphin
      peek # simple animated gif screen recorder
      sxiv
      leafpad
      onlyoffice-bin
      # xfce.xfce4-terminal
      xfce.orage #simple calendar
      xfce.parole #media player
      xfce.ristretto #image viewer
      xfce.xfce4-battery-plugin
      xfce.xfce4-appfinder
      xorg.xclock
      stalonetray
      imagemagick
      audacious
      xarchiver
      xbanish
      xorg.xwd
      ledger-live-desktop
      #ledger-udev-rules
      acpi
      #dunst
      xorg.xkill
      gparted
      nushell
      libnotify
      deadd-notification-center
      tint2
      dos2unix
      xdotool

      discord
      betterdiscord-installer
      betterdiscordctl
      steamPackages.steamcmd
      minecraft
    ];
  };
}
