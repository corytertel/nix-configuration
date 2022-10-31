{ config, pkgs, lib, ... }:

{
  imports = [
    ./udev.nix
    ./zfs.nix
  ];

  boot = {
    cleanTmpDir = true;
    loader = {
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        devices = ["nodev"];
        efiSupport = true;
        useOSProber = true;
      };
    };
  };

  networking = {
    hostName = "nixos";
    wireless.enable = false;  # disables wpa_supplicant
    useDHCP = false;
    networkmanager.enable = true;
  };

  time.timeZone = "America/Phoenix";

  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true;

  services = {
    flatpak.enable = true;
    openssh.enable = true;
    printing.enable = true;
    xserver = {
      enable = true;
      extraLayouts = {
        us_dvorak_emacs = {
          description = "US layout with numbers and characters flipped";
          languages = [ "eng" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/us_dvorak_emacs";
        };
        us_dvorak_emacs_kinesis = {
          description = "US layout with numbers and characters flipped";
          languages = [ "eng" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/us_dvorak_emacs_kinesis";
        };
        ru_dvorak = {
          description = "Russian implementation of dvorak layout";
          languages = [ "rus" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/ru_dvorak";
        };
        ru_phonetic_dvorak = {
          description = "Russian phonetic translation of the US dvorak emacs layout";
          languages = [ "rus" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/ru_phonetic_dvorak";
        };
      };
      libinput = {
        enable = true;
        mouse = {
          # Mouse
          accelProfile = "flat";
          accelSpeed = null;
          disableWhileTyping = true;

          # Trackball
          # accelProfile = "flat";
          # accelSpeed = null;
          # buttonMapping = "1 8 2 4 5 6 7 3 9";
          # disableWhileTyping = true;
          # naturalScrolling = true;
          # scrollButton = 3;
          # scrollMethod = "button";
          # transformationMatrix = "2.5 0 0 0 2.5 0 0 0 1";
        };
      };
    };
    # redshift = {
    #   enable = true;
    # };
  };

  location = {
    latitude = 33.4484;
    longitude = -112.0740;
  };

  systemd = {
    services.nix-gc.unitConfig.ConditionACPower = true;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users = {
    users = {
      cory = {
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "wheel" "network" "audio" "libvirtd" ];
        home = "/home/cory";
      };
    };
    extraGroups.vboxusers.members = [ "cory" ];
  };

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };
    settings = {
      auto-optimise-store = true;
      trusted-users = [ "root" "cory" ];
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
    system = "x86_64-linux";
  };

  environment = {
    variables = with pkgs; {
      BROWSER = config.apps.browser.command;
      CHICKEN_REPOSITORY_PATH = "${chicken-lsp-server}/lib/chicken/${toString chicken.binaryVersion}";
      CHICKEN_INCLUDE_PATH = "${chicken}/share";
    };
    systemPackages = with pkgs; [
      wget
      curl
      git
      gcc
      gnumake
      cmake
      which
      ripgrep
      keyboard-layouts
      trim-generations
    ];
  };

  fonts.fonts = with pkgs; [
    config.theme.font.system.package
    config.theme.font.monospace.package
    victor-mono
    oxygenfonts
    corefonts
    vistafonts
  ];

  virtualisation = {
    virtualbox.host.enable = true; # Virtual Box
    libvirtd.enable = true; # virt-manager
    # anbox.enable = true;
    waydroid.enable = true;
  };

  programs = {
    dconf.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "tty";
    };
    steam.enable = true;
  };

  xdg = {
    # portal = {
    #   enable = true;
    #   extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    # };
    mime.defaultApplications = let
      document = "writer.desktop";
      presentation = "impress.desktop";
      telegram = "telegramdesktop.desktop";
    in with config.apps; {
      "application/pdf" = pdfViewer.desktopFile;
      "x-scheme-handler/tg" = telegram;
      "application/x-sh" = terminal.desktopFile;
      "text/plain" = editor.desktopFile;
      "inode/directory" = fileManager.desktopFile;

      "application/zip" = archiver.desktopFile;
      "application/x-7z-compressed" = archiver.desktopFile;
      "application/vnd.rar" = archiver.desktopFile;
      "application/gzip" = archiver.desktopFile;

      "application/msword" = document;
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = document;
      "application/vnd.ms-powerpoint" = presentation;
      "application/vnd.openxmlformats-officedocument.presentationml.presentation" = presentation;

      "application/x-extension-htm" = browser.desktopFile;
      "application/x-extension-html" = browser.desktopFile;
      "application/x-extension-shtml" = browser.desktopFile;
      "application/x-extension-xht" = browser.desktopFile;
      "application/x-extension-xhtml" = browser.desktopFile;
      "application/xhtml+xml" = browser.desktopFile;
      "text/html" = browser.desktopFile;
      "x-scheme-handler/about" = browser.desktopFile;
      "x-scheme-handler/chrome" = browser.desktopFile;
      "x-scheme-handler/http" = browser.desktopFile;
      "x-scheme-handler/https" = browser.desktopFile;
      "x-scheme-handler/ftp" = browser.desktopFile;
      "x-scheme-handler/unknown" = browser.desktopFile;

      "audio/aac" = musicPlayer.desktopFile;
      "audio/mpeg" = musicPlayer.desktopFile;
      "audio/ogg" = musicPlayer.desktopFile;
      "audio/opus" = musicPlayer.desktopFile;
      "audio/wav" = musicPlayer.desktopFile;
      "audio/weba" = musicPlayer.desktopFile;

      "image/bmp" = photoViewer.desktopFile;
      "image/gif" = photoViewer.desktopFile;
      "image/ico" = photoViewer.desktopFile;
      "image/jpeg" = photoViewer.desktopFile;
      "image/png" = photoViewer.desktopFile;
      "image/svg" = photoViewer.desktopFile;
      "image/svg+xml" = photoViewer.desktopFile;
      "image/svg-xml" = photoViewer.desktopFile;
      "image/webp" = photoViewer.desktopFile;

      "video/mp4" = videoPlayer.desktopFile;
      "video/mpeg" = videoPlayer.desktopFile;
      "video/ogg" = videoPlayer.desktopFile;
      "video/webm" = videoPlayer.desktopFile;
      "video/x-msvideo" = videoPlayer.desktopFile;
      "video/quicktime" = videoPlayer.desktopFile;
      "video/x-matroska" = videoPlayer.desktopFile;
    };
  };

  system.stateVersion = "21.11";

  home-manager.users.cory = {
    programs = {
      home-manager.enable = true;

      nix-index.enable = true;

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

      packages = let
        tex = (pkgs.texlive.combine {
          inherit (pkgs.texlive) scheme-basic
            dvisvgm dvipng # for preview and export as html
            wrapfig amsmath ulem hyperref capt-of;
        });
      in with pkgs; [
        # linux basics
        killall
        btop

        # c++
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
        # irony-server

        # clojure
        clisp
        clojure
        clojure-lsp
        leiningen
        # joker
        clj-kondo
        # babashka

        # common lisp
        clisp
        sbcl
        asdf

        # chicken scheme
        chicken
        chicken-lsp-server
        egg2nix

        # racket
        racket
        # for lsp run:
        # raco pkg install racket-langserver

        # other programing languages
        nodejs
        yarn
        tex
        python39Full
        python39Packages.pip
        rnix-lsp
        javaPackages.openjfx17
        maven
        gradle

        # essential user apps
        tdesktop
        blender

        # modern unix
        fd
        ripgrep
        fzf
        jq

        nix-prefetch-github
        git-crypt
        bb
        qbittorrent
        # brave
        tree
        imagemagick
        yt-dlp
        # qemu
        # qutebrowser
        wine64
        winetricks
        grapejuice
        pciutils
        # peek # simple animated gif screen recorder
        # leafpad
        # onlyoffice-bin
        ledger-live-desktop
        #ledger-udev-rules
        acpi
        gparted
        libnotify
        dos2unix
        # galculator
        unzip
        klavaro
        obs-studio
        # okular
        libreoffice
        fd
        # citra-canary
        # protonvpn-gui
        inkscape
        thunderbird
        # mullvad-vpn
        # lazpaint
        libsForQt5.kcalc
        anki
        # anki-bin
        peek

        # games
        libsForQt5.kpat
        libsForQt5.kolf
        libsForQt5.kmines
        libsForQt5.kmahjongg
        libsForQt5.kapman
        libsForQt5.kspaceduel
        libsForQt5.knights
        libsForQt5.konquest
        libsForQt5.knavalbattle
        libsForQt5.ksudoku
        libsForQt5.killbots
        # ace-of-penguins
        superTux
        superTuxKart
        # srb2kart
        # crispyDoom

        mesa-demos

        # wireshark
        # tcpdump
        # nmap
      ];
    };
  };
}
