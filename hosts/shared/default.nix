{ config, pkgs, lib, ... }:

{
  imports = [
    ./fhs.nix
    ./homebrew.nix
    ./udev.nix
    ./wireguard.nix
    ./xterm.nix
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
        extraConfig = ''
          GRUB_CMDLINE_LINUX="reboot=bios"
        '';
      };
    };
  };

  networking = {
    hostName = "nixos";
    wireless.enable = false;  # disables wpa_supplicant
    useDHCP = false;
    networkmanager.enable = true;
    resolvconf.extraOptions = ["edns0"];

    firewall = {
      enable = true;
      # 8080 is tandoor
      allowedTCPPorts = [ 80 443 8080 ];
    };
  };

  time.timeZone = "America/Phoenix";

  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true;

  services = {
    flatpak.enable = true;
    openssh.enable = true;
    printing.enable = true;
    upower.enable = true;
    spice-vdagentd.enable = true; # virt-manager
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
    };

    libinput.enable = true;

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

        # IPv6 version of above
        host postgres all ::1/128 reject
        host all all ::1/128 scram-sha-256
      '';
      identMap = ''
        superuser_map root postgres
        superuser_map postgres postgres
      '';
    };

    # Mongodb service
    # mongodb = {
    #   enable = true;
    # };

    # Jellyfin
    jellyfin = {
      enable = true;
      openFirewall = true;
      dataDir = "/persist/jellyfin";
    };

    tandoor-recipes = {
      enable = true;
      # address = "192.168.50.241";
      address = "0.0.0.0";
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
  # sound.enable = false; # only meant for ALSA-based configs
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # jack.enable = true; # to use JACK applications
  };

  # TODO this should not be declarative
  users = {
    users = {
      cory = {
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "wheel" "network" "audio" "video" "libvirtd" "linuxbrew" ];
      };
    };
    extraGroups.vboxusers.members = [ "cory" ];
  };

  nix = {
    package = pkgs.nixVersions.latest;
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
      cores = 1;
      auto-optimise-store = true;
      trusted-users = [ "root" "cory" ];
    };
  };

  # Make /bin/bash exist so scripts work
  system.activationScripts.binbash = {
    text = ''
      ln -sfn ${pkgs.bashInteractive}/bin/bash /bin/bash
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
    variables = let
      makePluginPath = format:
        (lib.makeSearchPath format [
          "$HOME/.nix-profile/lib"
          "/run/current-system/sw/lib"
          "/etc/profile/per-user/$USER/lib"
        ])
        + ":$HOME/.${format}";
    in with pkgs; {
      PAGER = "less -S";
      BROWSER = config.apps.browser.command;

      CHICKEN_REPOSITORY_PATH =
        "${chicken-pkgs}/lib/chicken/${toString chicken.binaryVersion}";
      # CHICKEN_DOC_REPOSITORY = "${pkgs.chicken-docs}";

      # CLASSPATH = "${postgresql_jdbc}/share/java/postgresql-jdbc.jar";
      _JAVA_OPTIONS = "-Dawt.useSystemAAFontSettings=lcd";

      # Fix for DAW's expecting FHS paths
      DSSI_PATH = makePluginPath "dssi";
      LADSPA_PATh = makePluginPath "ladspa";
      LV2_PATH = makePluginPath "lv2";
      LXVST_PATH = makePluginPath "lxvst";
      VST_PATH = makePluginPath "vst";
      VST3_PATH = makePluginPath "vst3";
    };
    sessionVariables = with pkgs; {
      DOTNET_ROOT = "${dotnetCorePackages.sdk_8_0}";
    };
    systemPackages = with pkgs; [
      tmux
      vim
      nano
      wget
      curl
      git
      cmake
      ripgrep
      keyboard-layouts
      trim-generations

      # fhs dependencies
      tcsh
      ed
      coreutils-full
      util-linux
      procps
      gnutar
      cpio
      gzip
      nettools
      iputils

      # base devel
      autoconf
      automake
      binutils
      bison
      debugedit
      fakeroot
      file
      findutils
      flex
      gawk
      gcc
      gettext
      gnugrep
      groff
      gzip
      libtool
      gnum4
      gnumake
      gnupatch
      pkgconf
      gnused
      sudo
      texinfo
      which
    ];
  };

  fonts = with config.theme.font; {
    enableDefaultPackages = true;
    fontDir.enable = true;
    packages = with pkgs; [
      serif.package
      sansSerif.package
      monospace.package

      # For compatibility with Windows
      corefonts
      vistafonts

      # whatsapp-emoji-font
      libertinus
      uw-ttyp0
      courier-prime
      libre-franklin
      iosevka-comfy.comfy-motion-fixed
    ];
    # fontconfig = {
    #   enable = true;
    #   defaultFonts = {
    #     serif = [ "${serif.name}" ];
    #     sansSerif = [ "${sansSerif.name}" ];
    #     monospace = [ "${monospace.name}" ];
    #     # emoji = [ "Apple Color Emoji" ];
    #   };
    #   # FIXME apple emojis for discord
    #   # localConf = builtins.readFile ./fontconfig.xml;
    # };
  };

  virtualisation = {
    libvirtd = {
      enable = true;
      qemu = {
        swtpm.enable = true;
        ovmf.enable = true;
        ovmf.packages = [ pkgs.OVMFFull.fd ];
      };
    };
    spiceUSBRedirection.enable = true;
    # docker
    docker = { # docker
      enable = true;
      rootless = {
        enable = true;
        setSocketVariable = true;
      };
      daemon.settings = {
        data-root = "/persist/docker";
      };
    };
  };

  programs = {
    dconf.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    # steam.enable = true;
    firefox.enable = true;
    chromium.enable = true;
    partition-manager.enable = true;
  };

  xdg = {
    # portal = {
    #   enable = true;
    #   extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    # };
    portal = {
      enable = true;
      config.common.default = "*";
      # extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      extraPortals = [ pkgs.lxqt.xdg-desktop-portal-lxqt ];
      # extraPortals = [ pkgs.libsForQt5.xdg-desktop-portal-kde ];
    };
    mime.defaultApplications = let
      document = "writer.desktop";
      presentation = "impress.desktop";
      telegram = "telegramdesktop.desktop";
      archiver = "xarchiver.desktop";
      terminal = "xterm.desktop";
    in with config.apps; {
      # "application/pdf" = pdfViewer.desktopFile;
      "x-scheme-handler/tg" = telegram;
      # "application/x-sh" = terminal;
      # "text/plain" = editor.desktopFile;
      # "inode/directory" = fileManager.desktopFile;

      # "application/zip" = archiver;
      # "application/x-7z-compressed" = archiver;
      # "application/vnd.rar" = archiver;
      # "application/gzip" = archiver;

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

      # "audio/aac" = musicPlayer.desktopFile;
      # "audio/mpeg" = musicPlayer.desktopFile;
      # "audio/ogg" = musicPlayer.desktopFile;
      # "audio/opus" = musicPlayer.desktopFile;
      # "audio/wav" = musicPlayer.desktopFile;
      # "audio/weba" = musicPlayer.desktopFile;

      # "image/bmp" = photoViewer.desktopFile;
      # "image/gif" = photoViewer.desktopFile;
      # "image/ico" = photoViewer.desktopFile;
      # "image/jpeg" = photoViewer.desktopFile;
      # "image/png" = photoViewer.desktopFile;
      # "image/svg" = photoViewer.desktopFile;
      # "image/svg+xml" = photoViewer.desktopFile;
      # "image/svg-xml" = photoViewer.desktopFile;
      # "image/webp" = photoViewer.desktopFile;
      # "image/xpm" = photoViewer.desktopFile;

      # "video/mp4" = videoPlayer.desktopFile;
      # "video/mpeg" = videoPlayer.desktopFile;
      # "video/ogg" = videoPlayer.desktopFile;
      # "video/webm" = videoPlayer.desktopFile;
      # "video/x-msvideo" = videoPlayer.desktopFile;
      # "video/quicktime" = videoPlayer.desktopFile;
      # "video/x-matroska" = videoPlayer.desktopFile;
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
        extraConfig = {
          init = {
            defaultBranch = "master";
          };
        };
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
          # # Hardtime
          # {
          #   plugin = hardtime-nvim;
          #   type = "lua";
          #   config = ''require("hardtime").setup()'';
          # }
          # # Dependencies of Hardtime
          # nui-nvim
          # plenary-nvim
	        # Rainbow parenthesis
	        {
	          plugin = rainbow; config = "let g:rainbow_active = 1";
	        }
	        # # Matching parenthesis
	        # {
          #   plugin = auto-pairs;
          #   config = ''
	        #     let g:AutoPairsFlyMode = 0
	        #   '';
	        # }
          vim-surround
          vim-repeat
          vim-sexp
          # vim-sexp-mappings-for-regular-people
        ];
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
        pnpm
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
        # (dyalog.override { acceptLicense = true; })
        # dyalogscript
        # ride

        # python
        (python312.withPackages (ps: with ps; [
          epc
          python-lsp-server
          pygments

          pip
          numpy
          pandas
          scipy
          pyarrow

          opencv4
          scikit-image
          matplotlib

          tkinter
        ]))

        # postgres
        postgresql
        dbeaver-bin
        postgresql_jdbc # for java

        # smalltalk
        # squeak
        # gnu-smalltalk

        # perl
        # TODO figure out perl LSP
        (perl538.withPackages (ps: with ps; [
          # Install the default perl modules that come with RHEL
          Appcpanminus
          DBDmysql
          DBDPg
          DBDSQLite
          DBI
          FCGI
          YAML

          # Tk for fvwm (comes with it anyways)
          Tk
        ]))
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

        # c#
        # dotnetCorePackages.sdk_9_0 # includes both NETCore and AspNetCore
        dotnetCorePackages.sdk_8_0 # includes both NETCore and AspNetCore
        # (with dotnetCorePackages; combinePackages [
        #   sdk_8_0
        # ])
        omnisharp-roslyn
        # ilspycmd

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
        # xarchiver
        xournalpp
        # teams-for-linux
        cantata
        ardour
        vital

        # base devel

        # command line utils
        mg
        fd
        ripgrep
        jq
        yt-dlp
        unzip
        nix-prefetch-github
        imagemagick
        pciutils
        killall
        tokei
        tree
        xclip

        # security
        nmap
        wireshark
        tcpdump

        # xorg utils
        xorg.xhost
        xorg.xset
        xorg.xev

        # libs
        ffmpeg
        libnotify

        # virt-manager
        virt-manager
        virt-viewer
        spice
        spice-gtk
        spice-protocol
        win-virtio
        win-spice
        adwaita-icon-theme # can throw errors if does not exist

        # misc
        ledger-live-desktop
        #ledger-udev-rules
        nicotine-plus
        wine
        eclipses.eclipse-java

        acpi
        klavaro
        anki
        sshfs
        # has security issues rn
        # megasync
        chromium

        # apt
        # (stdenv.mkDerivation {
        #   name = "apt-combined";
        #   src = symlinkJoin {
        #     name = "apt-combined-src";
        #     paths = [
        #       apt
        #       dpkg
        #     ];
        #   };
        #   dontBuild = true;
        #   installPhase = ''
        #     mkdir -p $out
        #     cp -rL $src/* $out/
        #   '';
        # })

        # iphone utils
        ifuse
        libimobiledevice
      ];
    };
  };
}
