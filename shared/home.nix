{ config, pkgs, ... }:

{
  imports =
    [
      ./apps/bash
      ./apps/layout_switch
      ./apps/ksh
      ./apps/neofetch
      ./apps/nu
      #./apps/pcmanfm
      ./apps/powershell
      ./apps/ungoogled-chromium
      ./apps/urxvt
      ./apps/vscode
      ./apps/xfce4-terminal
    ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "cory";
  home.homeDirectory = "/home/cory";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  # Direnv
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  # Java
  programs.java = {
    enable = true;
    #jdk11 #openjdk11 for java
    #jdk #openjdk17 for java
    package = pkgs.jdk;
  };

  home.packages = with pkgs; [
   # linux basics
    killall
    #chezmoi
    kitty
    #vlc
    gwenview
    neofetch
    cmus
    #youtube-dl
    feh
    cmatrix
    #pipes
    dmenu
    networkmanager_dmenu
    #alacritty
    #slock
    #ranger
    bpytop
    atool
    libsForQt5.ark
    #tree-sitter

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

  # other programing languages
    python39Full
    python39Packages.pip
    rustc
    rust-analyzer
    clisp
    leiningen

   # essential user apps
    tdesktop
    flameshot
    libreoffice
    gimp
    #wpgtk
    blender
    zathura
    #joplin
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
    gnome.gnome-disk-utility
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
    xfce.xfce4-terminal
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

    discord
    betterdiscord-installer
    betterdiscordctl
    steamPackages.steamcmd
    minecraft
  ];
}
