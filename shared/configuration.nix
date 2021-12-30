{ config, pkgs, ... }:

{
  imports =
    [
      ./overlays.nix
      ../secrets/secrets.nix
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

  networking = {
    hostName = "nixos";
    wireless.enable = false;  # disables wpa_supplicant
    useDHCP = false;
    networkmanager.enable = true;
  };

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

