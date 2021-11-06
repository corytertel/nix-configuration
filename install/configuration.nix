# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Grub
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.enable = true;
  boot.loader.grub.devices = ["nodev"];
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.useOSProber = true;

  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Phoenix";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;
  networking.networkmanager.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Configure keymap in X11
  services.xserver = {
    exportConfiguration = true;
    enable = true;

    # Keyboard
    xkbModel = "microsoft";
    layout = "us,ru";
    xkbOptions = "grp:toggle"; # ralt toggle keyboard
    xkbVariant = "winkeys";

    # LightDM
    displayManager = {
      defaultSession = "xfce";
      lightdm.enable = true;
      lightdm.greeters.gtk = {
        enable = true;
      };
    }

    # Xfce
    desktopManager.xfce.enable = true;

    # Enable touchpad support (enabled default in most desktopManager).
    libinput = {
      enable = true;
      touchpad = {
        accelProfile = "adaptive";
        accelSpeed = null;
        additionalOptions = "";
        buttonMapping = null;
        calibrationMatrix = null;
        #sets trackpad to two-finger rightclick instead of button areas
        clickMethod = "clickfinger";
        dev = null;
        disableWhileTyping = false;
        horizontalScrolling = true;
        leftHanded = false;
        middleEmulation = false;
        naturalScrolling = true;
        scrollButton = null;
        scrollMethod = "twofinger";
        sendEventsMode = "enabled";
        tapping = false;
        tappingDragLock = false;
      };
    };
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.cory = {
    isNormalUser = true;
    extraGroups = [ "wheel" "network" ]; # Enable ‘sudo’ for the user.
  };

  # Enable flakes
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs.config.allowUnfree = true; 
  environment.systemPackages = with pkgs; [
   # install essentials
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    curl
    firefox
    git
    networkmanager
    networkmanagerapplet
    dialog
    wpa_supplicant
    mtools
    dosfstools
   # base development tools 
    autoconf
    automake
    binutils
    bison
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
    cmake
    gnumake
    pacman
    gnupatch
    pkgconf
    sedutil
    sudo
    texinfo
    which
   # end base devel tools
    linuxHeaders
    avahi
    bluez
    cups
    pulseaudio
    pavucontrol
    pamixer
    alsaUtils
    os-prober
    ntfs3g
   # end install essentials

   # linux basics
    killall
    neovim 
    kitty
    neofetch

    # other essentials
    nix-prefetch-github
  ];

  # Fonts
  fonts.fonts = with pkgs; [
    roboto-mono
    (nerdfonts.override { fonts = ["RobotoMono"]; })
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

