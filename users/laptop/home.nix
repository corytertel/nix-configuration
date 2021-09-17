{ config, pkgs, ... }:

{
  imports =
    [
      ./apps/nvim
      ./apps/kitty
      ./apps/bash
      ./apps/vscode
      #./apps/emacs
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

  home.packages = with pkgs; [
   # linux basics
    killall
    chezmoi
    kitty
    vlc
    gwenview
    neofetch
    cmus
    youtube-dl
    feh
    cmatrix
    pipes
    dmenu
    networkmanager_dmenu
    picom
    #dunst
    alacritty
    slock
    ranger
    bpytop
    atool
    libsForQt5.ark
    nodejs
    yarn

  # development basics
    ccls
    clang_12
    #llvmPackages_12.clang-unwrapped # clangd
    clang-tools # clang-tidy and clang-analyzer
    #libclang
    llvmPackages_12.libcxx
    #clang-analyzer
    cppcheck
    ncurses
    global
    gdb

  # other programing languages
    jdk11 #openjdk11 for java
    python39Full
    python39Packages.pip

   # essential user apps
    discord
    #steam
    tdesktop
    flameshot
    libreoffice
    gimp
    #wpgtk
    #anbox
    blender
    zathura
    joplin
    joplin-desktop

    # other essentials
    nerdfonts
    roboto-mono
    libsForQt5.krohnkite
    betterdiscord-installer
    betterdiscordctl
    nix-prefetch-github
    #haskellPackages.xmobar
    libsForQt5.plasma-applet-virtual-desktop-bar
    #latte-dock
    #libsForQt5.qtstyleplugin-kvantum
    #conky
    git-crypt
    gnupg
    cool-retro-term
    bat
    bb
    htop
    qbittorrent
    audacity
    protonvpn-gui
    protonvpn-cli
    polkit
    libsForQt5.polkit-qt
    brave
    celluloid
    gnome.gnome-disk-utility
    steamPackages.steamcmd
    steam-tui
    mpv
  ];

  home.file = {
    ".Xresources".text = builtins.readFile ./system/Xresources;
  };
}
