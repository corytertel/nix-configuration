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

  apps.editor = {
    name = "emacs";
    command = "emacsclient -c";
    desktopFile = "emacsclient.desktop";
    package = pkgs.emacsGcc;
  };

  environment = {
    variables = {
      ALTERNATE_EDITOR = "emacs -nw";
      EDITOR = "emacsclient -nw";
      VISUAL = "emacsclient -c -a ''";
      BROWSER = config.apps.browser.command;
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
    ];
  };

  fonts.fonts = with pkgs; [
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
    dconf.enable = true;
    gnupg.agent = {
     enable = true;
     enableSSHSupport = true;
     pinentryFlavor = "tty";
    };
    steam.enable = true;
  };

  xdg = {
    portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };
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
          #(setq org-latex-compiler "lualatex")
          #(setq org-preview-latex-default-process 'dvisvgm)
        });
      in with pkgs; [
        # linux basics
        killall
        btop

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
        peek # simple animated gif screen recorder
        # leafpad
        # onlyoffice-bin
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
        libreoffice-qt
        fd
        citra-canary
        # protonvpn-gui
        inkscape
        thunderbird

        # Games
        libsForQt5.kpat
        libsForQt5.kolf
        libsForQt5.kmines

        wireshark
        tcpdump
        nmap
      ];
    };
  };
}
