{ config, pkgs, ... }:

{
  imports = let
    commit = "1514ac9fd54363a24c513de43dd0b963e2d17cb7";
  in [
    "${builtins.fetchTarball {
      url = "https://github.com/Mic92/sops-nix/archive/${commit}.tar.gz";
      sha256 = "0dfgg0mysjhlfr3vjklcshlvywzm6kk9qx5bbjmbz6c5p10wi8g2";
    }}/modules/sops"

    ./overlays.nix
    ./udev.nix
  ];

  sops.defaultSopsFile = ../secrets/secrets.yaml;
  sops.age.keyFile = "/home/cory/.config/sops/age/keys.txt";
  sops.age.generateKey = true;
  sops.secrets.root.neededForUsers = true;
  sops.secrets.cory.neededForUsers = true;

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
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services = {
    emacs.enable = true;
    openssh.enable = true;
    pcscd.enable = true;
    printing.enable = true;
    xserver = {
      enable = true;
      exportConfiguration = true;
      xkbModel = "microsoft";
      layout = "us,ru";
      xkbOptions = "grp:toggle"; # ralt toggle keyboard
      xkbVariant = "winkeys";
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
    user.services.emacs.unitConfig.ConditionGroup = "users"; # only start emacs for actual users
    services.nix-gc.unitConfig.ConditionACPower = true;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users = {
    mutableUsers = false;
    users.cory = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "network" "audio" "libvirtd" ];
      createHome = true;
      home = "/home/cory";
      shell = pkgs.ksh;
    };
    extraGroups.vboxusers.members = [ "cory" ];

    users.root.passwordFile = config.sops.secrets.root.path;
    users.cory.passwordFile = config.sops.secrets.cory.path;
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

