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
    upower.enable = true;
    xserver = {
      enable = true;
      xkb.extraLayouts = {
        us_qwerty = {
          description = "US standard layout";
          languages = [ "eng" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/us_qwerty";
        };
        us_dvorak = {
          description = "US standard layout";
          languages = [ "eng" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/us_dvorak";
        };
        us_dvorak_iso = {
          description = "US standard layout";
          languages = [ "eng" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/us_dvorak_iso";
        };
        ru_phonetic_qwerty = {
          description = "Russian phonetic translation of the US qwerty layout";
          languages = [ "rus" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/ru_phonetic_qwerty";
        };
        ru_phonetic_dvorak = {
          description = "Russian phonetic translation of the US dvorak layout";
          languages = [ "rus" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/ru_phonetic_dvorak";
        };
        ru_phonetic_dvorak_iso = {
          description = "Russian phonetic translation of the US dvorak layout";
          languages = [ "rus" ];
          symbolsFile = "${pkgs.keyboard-layouts}/share/X11/xkb/symbols/ru_phonetic_dvorak_iso";
        };
      };
      libinput.enable = true;
    };

    # Postgres service
    postgresql = {
      enable = true;
      package = pkgs.postgresql_15;
      dataDir = "/persist/postgresql/data";
      authentication = lib.mkOverride 10 ''
        # Must allow at least postgres local peer connection for those tha pass the ident check
        local postgres all peer map=superuser_map

        # Allow local users to connect to any database, but postgres, if the password is correct.
        local postgres all reject
        local all all scram-sha-256

        # Allow localhost users to connect to any database, but postgres, if the password is correct.
        host postgres all 127.0.0.1/32 reject
        host all all 127.0.0.1/32 scram-sha-256
      '';
      identMap = ''
        superuser_map root postgres
        superuser_map postgres postgres
      '';
    };
  };

  location = {
    latitude = 33.4484;
    longitude = -112.0740;
  };

  systemd = {
    services.nix-gc.unitConfig.ConditionACPower = true;
  };

  # # Pulseaudio
  # sound.enable = true;
  # hardware.pulseaudio = {
  #   enable = true;
  #   # For the qpaeq equalizer
  #   extraConfig = ''
  #     load-module module-equalizer-sink
  #     load-module module-dbus-protocol
  #   '';
  # };
  # Pipewire
  sound.enable = false; # only meant for ALSA-based configs
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # jack.enable = true; # to use JACK applications
  };

  users = {
    users = {
      cory = {
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "wheel" "network" "audio" "video" "libvirtd" ];
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

  # Make /bin/bash exist so scripts work
  system.activationScripts.binbash = {
    text = ''
      ln -sfn ${pkgs.bash}/bin/bash /bin/bash
    '';
  };

  # Create postgres folder
  # system.activationScripts.postgres = {
  #   text = ''
  #     mkdir -p /persist/postgresql
  #     chown postgres:postgres /persist/postgresql
  #   '';
  # };

  environment = {
    variables = with pkgs; {
      PAGER = "less -S";
      BROWSER = config.apps.browser.command;
      # CLASSPATH = "${postgresql_jdbc}/share/java/postgresql-jdbc.jar";
      CHICKEN_REPOSITORY_PATH =
        "${chicken-pkgs}/lib/chicken/${toString chicken.binaryVersion}";
      # CHICKEN_DOC_REPOSITORY = "${pkgs.chicken-docs}";
      _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=lcd";
      # Display matplotlib in kitty
      MPLBACKEND = "module://matplotlib-backend-kitty";
      MPLBACKEND_KITTY_SIZING = "manual";
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

  fonts = with config.theme.font; {
    enableDefaultPackages = true;
    fontDir.enable = true;
    packages = with pkgs; [
      serif.package
      sansSerif.package
      monospace.package
      corefonts
      vistafonts
      whatsapp-emoji-font
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        serif = [ "${serif.name}" ];
        sansSerif = [ "${sansSerif.name}" ];
        monospace = [ "${monospace.name}" ];
        emoji = [ "Apple Color Emoji" ];
      };
      # FIXME apple emojis for discord
      localConf = builtins.readFile ./fontconfig.xml;
    };
  };

  virtualisation = {
    virtualbox.host.enable = true; # Virtual Box
    libvirtd.enable = true; # virt-manager
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
      "image/xpm" = photoViewer.desktopFile;

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

      neovim = {
        enable = true;
	      extraLuaConfig = ''
	        vim.wo.relativenumber = true
	      '';
        plugins = with pkgs.vimPlugins; [
          # Emacs colorscheme
          {
            plugin = vim-colorschemes;
            config = ''
              colorscheme emacs
              highlight Visual ctermfg=Black ctermbg=Yellow
            '';
          }
          # Hardtime
          {
            plugin = hardtime-nvim;
            type = "lua";
            config = ''require("hardtime").setup()'';
          }
          # Dependencies of Hardtime
          nui-nvim
          plenary-nvim
	  # Rainbow parenthesis
	  {
	    plugin = rainbow; config = "let g:rainbow_active = 1";
	  }
	  # Matching parenthesis
	  {
            plugin = auto-pairs;
            config = ''
	      let g:AutoPairsFlyMode = 0
	    '';
	  }
        ];
      };
    };

    services = {
      # Music
      # Needs to run as user, not system-wide, or else audio is played as root and unreachable
      mopidy = {
        enable = true;
        extensionPackages = with pkgs; [
          mopidy-mpd
          mopidy-local
          mopidy-soundcloud
        ];
        settings = {
          # file = {
          #   media_dirs = [
          #     "~/Music|Library"
          #   ];
          #   follow_symlinks = true;
          #   excluded_file_extensions = [
          #     ".html"
          #     ".zip"
          #     ".jpg"
          #     ".jpeg"
          #     ".png"
          #   ];
          # };

          # audio = {
          #   mixer = ''software'';
          #   output = ''autoaudiosink'';
          # };

          mpd = {
            enabled = true;
            port = 6600;
            # zeroconf = "Mopidy server on $hostname";
            # default_playlist_scheme = "m3u";
          };

          # local = {
          #   enabled = true;
          #   media_dir = "~/Music";
          #   excluded_file_extensions = ''
          #     .directory
          #     .html
          #     .jpeg
          #     .jpg
          #     .log
          #     .nfo
          #     .png
          #     .txt
          #   '';
          # };

          soundcloud = {
            enabled = true;
            auth_token = "3-35204-181561843-jUBNEp7hTDGbFwY";
            explore_songs = 100;
          };
        };
      };
    };

    home = {
      username = "cory";
      homeDirectory = "/home/cory";

      # TODO
      # Make nix-shell work with nixpkgs from system flake
      # sessionVariables.NIX_PATH = "nixpkgs=${pkgs.outPath}";

      packages = let
        dyalogscript = import ./dyalogscript.nix {inherit pkgs;};
      in with pkgs; [
        # c/c++
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
        global
        gdb
        # irony-server

        # common lisp
        clisp
        sbcl
        asdf

        # scheme
        chicken
        chicken-pkgs
        # chicken-docs
        tcl
        lips

        # racket
        racket

        # web
        nodejs
        yarn
        nodePackages_latest.typescript-language-server

        # java
        javaPackages.openjfx17
        jdt-language-server
        maven
        gradle

        # nix
        # rnix-lsp
        nil

        # apl
        dyalog
        dyalogscript
        # ride

        # python
        (python311.withPackages (ps: with ps;
          let
            qgis = buildPythonPackage {
              pname = "qgis";
              version = "3.36.0";
              src = "${pkgs.qgis}/share/qgis/python/qgis/";
            };

            matplotlib-backend-kitty = buildPythonPackage rec {
              pname = "matplotlib-backend-kitty";
              version = "2.1.2";
              format = "pyproject";
              src = fetchPypi {
                inherit pname version;
                sha256 = "sha256-q30X7wAYbGgKmJr4yb6fyJAKUF9cEH3H2avGexpX8VA=";
              };
              propagatedBuildInputs = [
                matplotlib
                setuptools
              ];
            };
          in [
            epc
            python-lsp-server
            pygments
            pip
            numpy
            pandas
            scipy

            flask
            flask-wtf
            python-dotenv

            opencv4
            scikit-image
            matplotlib
            matplotlib-backend-kitty

            # qgis
          ]))

        # postgres
        postgresql
        dbeaver
        postgresql_jdbc # for java

        # smalltalk
        squeak
        gnu-smalltalk

        # powershell
        # powershell

        # perl
        # TODO figure out perl LSP
        # (perl538.withPackages (ps: with ps; [
        # For web scraping
        #   HTMLTiny
        #   HTMLTree # includes HTMLTreeBuilder
        #   IOSocketSSL
        #   NetSSLeay

        #   # LSP and dependencies
        #   PerlLanguageServer
        #   AnyEventAIO
        #   Coro
        #   AnyEvent
        #   ClassRefresh
        #   CompilerLexer
        #   DataDump
        #   IOAIO
        #   JSON
        #   Moose
        #   PadWalker
        #   ScalarListUtils
        #   HashSafeKeys
        # ]))

        # desktop apps
        tdesktop
        photogimp
        blender
        kolourpaint
        inkscape
        libreoffice
        qalculate-gtk
        thunderbird
        obs-studio
        # peek
        drawio
        zoom-us
        qbittorrent
        xarchiver
        xournalpp
        teams-for-linux
        cantata

        # command line utils
        mg
        fd
        ripgrep
        file
        jq
        yt-dlp
        unzip
        nix-prefetch-github
        imagemagick
        pciutils
        killall
        tokei
        vim
        nano

        # libs
        ffmpeg
        libnotify

        # misc
        ledger-live-desktop
        #ledger-udev-rules
        wine64
        winetricks
        grapejuice

        acpi
        klavaro
        anki
        sshfs
        firefox
        chromium
        # has security issues rn
        # megasync

        # iphone utils
        ifuse
        libimobiledevice
      ];
    };
  };
}
