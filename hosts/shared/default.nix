{ config, pkgs, lib, ... }:

{
  imports = [
    ./udev.nix
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

  # Zfs
  boot.supportedFilesystems = [ "zfs" ];
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/local/root@blank
  '';
  boot.zfs.enableUnstable = true;
  boot.loader.grub.copyKernels = true;

  #services.zfs.autoScrub.enable = true;
  #services.zfs.autoScrub.interval = "weekly";
  #systemd.services.zfs-scrub.unitConfig.ConditionACPower = true;

  # Erase on every boot fix
  environment.etc = {
    "NetworkManager/system-connections".source = "/persist/etc/NetworkManager/system-connections/";
    "nixos".source = "/persist/etc/nixos/";
  };
  fileSystems."/persist".neededForBoot = true;

  networking = {
    hostName = "nixos";
    wireless.enable = false;  # disables wpa_supplicant
    useDHCP = false;
    networkmanager.enable = true;
  };

  time.timeZone = "America/Phoenix";

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
    # keyMap = "us_programmer";
  };

  services = {
    flatpak.enable = true;
    openssh.enable = true;
    pcscd.enable = true;
    printing.enable = true;
    xserver = {
      enable = true;
      extraLayouts = {
        us_programmer = {
          description = "US layout with numbers and characters flipped";
          languages = [ "eng" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/us_programmer";
        };
        ru_programmer = {
          description = "RU layout with numbers and characters flipped";
          languages = [ "rus" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/ru_programmer";
        };
      };
      layout = "us_programmer";
      libinput = {
        enable = true;
        mouse = {
          # Mouse
          accelProfile = "flat";
          accelSpeed = null;
          disableWhileTyping = true;

          # Trackball
          # accelProfile = "flat";
          # buttonMapping = "1 8 2 4 5 6 7 3 9";
          # disableWhileTyping = true;
          # naturalScrolling = true;
          # scrollButton = 3;
          # scrollMethod = "button";
          # transformationMatrix = "3 0 0 0 3 0 0 0 1";
        };
      };
    };
  };

  systemd = {
    services.nix-gc.unitConfig.ConditionACPower = true;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users = {
    mutableUsers = false;
    users = {
      root.passwordFile = "/persist/secrets/root";
      cory = {
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "wheel" "network" "audio" "libvirtd" ];
        createHome = true;
        home = "/home/cory";
        shell = pkgs.zsh;
        passwordFile = "/persist/secrets/cory";
      };
    };
    extraGroups.vboxusers.members = [ "cory" ];
  };

  security = {
    sudo = {
      enable = true;
      extraConfig = "Defaults lecture=never";
    };
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
    autoOptimiseStore = true;
    optimise = {
      automatic = true;
      dates = [ "weekly" ];
    };
    trustedUsers = [ "root" "cory" ];
  };

  nixpkgs = {
    config.allowUnfree = true;
    system = "x86_64-linux";
  };

  environment = {
    shells = [ pkgs.ksh pkgs.nushell ];
    variables = {
      EDITOR = "emacs -nw";
      # PLASMA_USE_QT_SCALING = "1";
      # QT_SCALE_FACTOR = "0.85";
    };
    systemPackages = with pkgs; [
      emacsGcc
      wget
      curl
      git
      gcc
      gnumake
      cmake
      which
      ripgrep
      keyboard-layouts
    ];
  };

  fonts.fonts = with pkgs; [
    julia-mono
    overpass
    junicode

    config.theme.font.system.package
    config.theme.font.monospace.package
  ];

  virtualisation = {
    virtualbox.host.enable = true; # Virtual Box
    libvirtd.enable = true; # virt-manager
    # anbox.enable = true;
    waydroid.enable = true;
  };

  programs = {
    cory.qpdfview.enable = true;
    dconf.enable = true;
    gnupg.agent = {
     enable = true;
     enableSSHSupport = true;
     pinentryFlavor = "tty";
    };
    steam.enable = true;
    zsh = {
      enable = true;
      enableCompletion = true;
      enableGlobalCompInit = true;
      autosuggestions.enable = true;
      setOptions = [];
      shellAliases = {};
      syntaxHighlighting.enable = true;
    };
  };

  xdg = {
    portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };
    mime.defaultApplications = let
      archiver = "lxqt-archiver.desktop";
      # archiver = "org.kde.ark.desktop";
      browser = "firefox.desktop";
      document = "writer.desktop";
      editor = "emacsclient.desktop";
      file-manager = "pcmanfm-qt.desktop";
      image = "lximage-qt.desktop";
      # image = "sxiv.desktop";
      # image = "org.kde.gwenview.desktop";
      music = "audacious.desktop";
      # pdf = "org.pwmt.zathura.desktop";
      # pdf = "okularApplication_pdf.desktop";
      pdf = "qpdfview.desktop";
      presentation = "impress.desktop";
      telegram = "telegramdesktop.desktop";
      terminal = "rxvt-unicode-client.desktop";
      video = "vlc.desktop";
    in {
      "application/pdf" = pdf;
      "x-scheme-handler/tg" = telegram;
      "application/x-sh" = terminal;
      "text/plain" = editor;
      "inode/directory" = file-manager;

      "application/zip" = archiver;
      "application/x-7z-compressed" = archiver;
      "application/vnd.rar" = archiver;
      "application/gzip" = archiver;

      "application/msword" = document;
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = document;
      "application/vnd.ms-powerpoint" = presentation;
      "application/vnd.openxmlformats-officedocument.presentationml.presentation" = presentation;

      "application/x-extension-htm" = browser;
      "application/x-extension-html" = browser;
      "application/x-extension-shtml" = browser;
      "application/x-extension-xht" = browser;
      "application/x-extension-xhtml" = browser;
      "application/xhtml+xml" = browser;
      "text/html" = browser;
      "x-scheme-handler/about" = browser;
      "x-scheme-handler/chrome" = browser;
      "x-scheme-handler/http" = browser;
      "x-scheme-handler/https" = browser;
      "x-scheme-handler/unknown" = browser;

      "audio/aac" = music;
      "audio/mpeg" = music;
      "audio/ogg" = music;
      "audio/opus" = music;
      "audio/wav" = music;
      "audio/weba" = music;

      "image/bmp" = image;
      "image/gif" = image;
      "image/ico" = image;
      "image/jpeg" = image;
      "image/png" = image;
      "image/svg" = image;
      "image/webp" = image;

      "video/mp4" = video;
      "video/mpeg" = video;
      "video/ogg" = video;
      "video/webm" = video;
      "video/x-msvideo" = video;
      "video/quicktime" = video;
      "video/x-matroska" = video;
    };
  };

  system.stateVersion = "21.11";

  home-manager.users.cory = {
    programs = {
      home-manager.enable = true;

      direnv.enable = true;
      direnv.nix-direnv.enable = true;

      nushell = {
        enable = true;
        settings = import ../../config/nushell;
      };

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
        racket
        rnix-lsp

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

        wireshark
        tcpdump
        nmap
      ];
    };
  };
}
