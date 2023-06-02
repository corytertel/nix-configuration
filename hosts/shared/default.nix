{ config, pkgs, lib, ... }:

{
  imports = [
    ./udev.nix
    ./zfs.nix
  ];

  boot = {
    tmp.cleanOnBoot = true;
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
        us_qwerty = {
          description = "US layout with numbers and characters flipped";
          languages = [ "eng" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/us_qwerty";
        };
        us_beakl_emacs = {
          description = "US layout with numbers and characters flipped";
          languages = [ "eng" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/us_beakl_emacs";
        };
        us_minimak = {
          description = "US layout with numbers and characters flipped";
          languages = [ "eng" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/us_minimak";
        };
        us_carplax = {
          description = "US layout with numbers and characters flipped";
          languages = [ "eng" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/us_carplax";
        };
        us_qgmlfj = {
          description = "US layout with numbers and characters flipped";
          languages = [ "eng" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/us_qgmlfj";
        };
        ru_phonetic_qwerty = {
          description = "Russian phonetic translation of the US qwerty layout";
          languages = [ "rus" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/ru_phonetic_qwerty";
        };
        ru_phonetic_minimak = {
          description = "Russian phonetic translation of the US minimak layout";
          languages = [ "rus" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/ru_phonetic_minimak";
        };
        ru_beakl_emacs = {
          description = "Russian phonetic translation of the US beakl emacs layout";
          languages = [ "rus" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/ru_beakl_emacs";
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
    postgresql = {
      enable = true;
      package = pkgs.postgresql;
      dataDir = "/persist/postgresql/data";
      authentication = lib.mkForce ''
        # Generated file; do not edit!
        local all all              trust
        host  all all 127.0.0.1/32 md5
        host  all all ::1/128      md5
      '';
    };
    # pgadmin =  {
    #   enable = true;
    #   initialEmail = "pgadmin4@pgadmin.org";
    #   initialPasswordFile = "/persist/secrets/pgadmin4";
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
      # CLASSPATH = "${postgresql_jdbc}/share/java/postgresql-jdbc.jar";
      CHICKEN_REPOSITORY_PATH =
        "${chicken-pkgs}/lib/chicken/${toString chicken.binaryVersion}";
      # CHICKEN_INCLUDE_PATH = "${chicken}/share";
      _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=lcd";
    };
    sessionVariables = with pkgs; {
      DOTNET_ROOT = "${dotnet-sdk_7}";
    };
    systemPackages = with pkgs; [
      mg
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
    corefonts
    vistafonts
    liberation_ttf
    dejavu_fonts
    julia-mono
  ];

  virtualisation = {
    virtualbox.host.enable = true; # Virtual Box
    libvirtd.enable = true; # virt-manager
    # anbox.enable = true;
    # waydroid.enable = true;
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
      archiver = "xarchiver.desktop";
    in with config.apps; {
      "application/pdf" = pdfViewer.desktopFile;
      "x-scheme-handler/tg" = telegram;
      "application/x-sh" = terminal.desktopFile;
      "text/plain" = editor.desktopFile;
      "inode/directory" = fileManager.desktopFile;

      "application/zip" = archiver;
      "application/x-7z-compressed" = archiver;
      "application/vnd.rar" = archiver;
      "application/gzip" = archiver;

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

      packages = with pkgs; [
        # repeat of system basics
        mg

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

        # scheme
        chez
        chicken
        chicken-pkgs

        # racket
        racket
        # for lsp run:
        # raco pkg install racket-langserver

        # F#
        # fsharp
        # dotnet-sdk_7
        # libgccjit
        # dotnet-runtime
        # dotnet-aspnetcore
        # (with dotnetCorePackages; combinePackages [
        #   sdk_7_0
        # ])

        # web
        nodejs
        # yarn
        # yarn2nix
        nodePackages_latest.typescript-language-server

        # java
        javaPackages.openjfx17
        jdt-language-server
        maven
        gradle

        # other programing
        # rnix-lsp
        # nil
        gnuapl
        dyalog
        # ride
        python39
        # (python39.withPackages (ps: [ ps.epc ps.python-lsp-server ]))
        # python39Full
        # python39Packages.pip
        # python39Packages.python-lsp-server

        # postgres
        postgresql
        dbeaver
        postgresql_jdbc # for java

        # desktop apps
        tdesktop
        photogimp
        blender
        kolourpaint
        inkscape

        # utilities
        xarchiver

        fd
        ripgrep

        nix-prefetch-github
        git-crypt
        bb
        qbittorrent
        tree
        imagemagick
        yt-dlp
        ffmpeg
        wine64
        winetricks
        grapejuice
        pciutils
        peek # simple animated gif screen recorder
        ledger-live-desktop
        #ledger-udev-rules
        acpi
        gparted
        libnotify
        dos2unix
        unzip
        klavaro
        obs-studio
        libreoffice
        fd
        thunderbird
        qalculate-gtk
        anki
        sshfs
        drawio
        zoom-us
        firefox
        chromium
        zeal # offline documentation
        android-studio
      ];
    };
  };
}
