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
    openssh.enable = true;
    pcscd.enable = true;
    printing.enable = true;
    xserver = {
      enable = true;
      layout = "us";
      #exportConfiguration = true;
      #xkbModel = "microsoft";
      #layout = "us,ru";
      #xkbOptions = "grp:toggle"; # ralt toggle keyboard
      #xkbVariant = "winkeys";
      libinput = {
        enable = true;
        mouse = {
          accelProfile = "flat";
          accelSpeed = null;
          disableWhileTyping = true;
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
        shell = pkgs.ksh;
        passwordFile = "/persist/secrets/cory";
      };
    };
    extraGroups.vboxusers.members = [ "cory" ];
  };

  security = {
    sudo.enable = false;
    doas = {
      enable = true;
      extraRules = [{
        users = [ "cory" ];
        keepEnv = true;
        persist = true;
      }];
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
    shells = [ pkgs.ksh ];
    variables.EDITOR = "emacsclient -nw";
    systemPackages = with pkgs; [
      vim
      emacs
      wget
      curl
      firefox
      git
      networkmanager
      gcc
      gnumake
      cmake
      which
      fd
      htop
      pamixer
      ntfs3g
      git-crypt
      gnupg
      pinentry-gtk2
      age
      sops
    ];
  };

  fonts.fonts = with pkgs; [
    nerdfonts
  ];

  virtualisation = {
    virtualbox.host.enable = true; # Virtual Box
    libvirtd.enable = true; # virt-manager
  };

  programs = {
    dconf.enable = true;
    steam.enable = true;
    gnupg.agent = {
     enable = true;
     enableSSHSupport = true;
     pinentryFlavor = "gtk2";
   };
  };

  system.stateVersion = "21.05";
}

