{ config, pkgs, lib, ... }:

{
  imports = [
    ./overlays.nix
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
          symbolsFile = ./us_programmer;
        };
        ru_programmer = {
          description = "RU layout with numbers and characters flipped";
          languages = [ "rus" ];
          symbolsFile = ./ru_programmer;
        };
      };
      layout = "us_programmer";
      # xkbOptions = "caps:backspace,shift:both_capslock,altwin:ctrl_alt_win";
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
      dates = "*:0/10";
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
    system = "x86_64-linux";
  };

  environment = {
    shells = [ pkgs.ksh pkgs.nushell ];
    variables.EDITOR = "emacs -nw";
    systemPackages = with pkgs; [
      emacsGcc
      vim
      wget
      curl
      #firefox
      git
      networkmanager
      gcc
      gnumake
      cmake
      which
      fd
      htop
      pamixer
      ripgrep
      git-crypt
      gnupg
      pinentry
      age
      sops
      discord
    ];
  };

  fonts.fonts = with pkgs; [
    nerdfonts
    julia-mono
    jetbrains-mono
    # mplus-outline-fonts
    overpass
    #iosevka
    roboto-mono
    victor-mono
    junicode
  ];

  virtualisation = {
    virtualbox.host.enable = true; # Virtual Box
    libvirtd.enable = true; # virt-manager
    # anbox.enable = true;
  };

  programs = {
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
    mime.defaultApplications = {
      "application/pdf" = "org.pwmt.zathura.desktop";
      # "application/pdf" = "okularApplication_pdf.desktop";
      "x-scheme-handler/tg" = "telegramdesktop.desktop";
      "application/x-sh" = "rxvt-unicode.desktop";
      "text/plain" = "leafpad.desktop";
      "inode/directory" = "pcmanfm-qt.desktop";

      "application/zip" = "org.kde.ark.desktop";
      "application/x-7z-compressed" = "org.kde.ark.desktop";
      "application/vnd.rar" = "org.kde.ark.desktop";
      "application/gzip" = "org.kde.ark.desktop";

      "application/msword" = "onlyoffice-desktopeditors.desktop";
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "onlyoffice-desktopeditors.desktop";
      "application/vnd.ms-powerpoint" = "onlyoffice-desktopeditors.desktop";
      "application/vnd.openxmlformats-officedocument.presentationml.presentation" = "onlyoffice-desktopeditors.desktop";

      "application/x-extension-htm" = "firefox.desktop";
      "application/x-extension-html" = "firefox.desktop";
      "application/x-extension-shtml" = "firefox.desktop";
      "application/x-extension-xht" = "firefox.desktop";
      "application/x-extension-xhtml" = "firefox.desktop";
      "application/xhtml+xml" = "firefox.desktop";
      "text/html" = "firefox.desktop";
      "x-scheme-handler/about" = "firefox.desktop";
      "x-scheme-handler/chrome" = "firefox.desktop";
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
      "x-scheme-handler/unknown" = "firefox.desktop";

      "audio/aac" = "audacious.desktop";
      "audio/mpeg" = "audacious.desktop";
      "audio/ogg" = "audacious.desktop";
      "audio/opus" = "audacious.desktop";
      "audio/wav" = "audacious.desktop";
      "audio/weba" = "audacious.desktop";

      "mage/bmp" = "feh.desktop";
      "image/gif" = "org.kde.gwenview.desktop";
      "image/ico" = "feh.desktop";
      "image/jpeg" = "feh.desktop";
      "image/png" = "feh.desktop";
      "image/svg" = "feh.desktop";
      "image/webp" = "feh.desktop";

      "video/mp4" = "vlc.desktop";
      "video/mpeg" = "vlc.desktop";
      "video/ogg" = "vlc.desktop";
      "video/webm" = "vlc.desktop";
      "video/x-msvideo" = "vlc.desktop";
      "video/quicktime" = "vlc.desktop";
      "video/x-matroska" = "vlc.desktop";
    };
  };

  system.stateVersion = "21.11";
}
