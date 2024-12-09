# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # Dual booting with GRUB
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.enable = true;
  boot.loader.grub.devices = ["nodev"];
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.useOSProber = true;
  boot.cleanTmpDir = true;

  # Zfs
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "<GENERATE 8 CHAR RANDOM ID WITH `head -c 8 /etc/machine-id`>";
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

  # Kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "pcie_aspm.policy=performance"
    "mitigations=off"
    "nohibernate"
  ];

  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Phoenix";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s3.useDHCP = true;
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

  # Enable the X11 windowing system.
  # Configure keymap in X11
  services.xserver = {
    exportConfiguration = true;
    enable = true;
    xkbModel = "microsoft";
    layout = "us,ru";
    xkbOptions = "grp:toggle";
    xkbVariant = "winkeys";
  };

  # Enable FVWM and LightDM
  services.xserver = {
    displayManager = {
      defaultSession = "none+fvwm";
      gdm.enable = true;
      #lightdm.greeters.mini = {
      #  enable = true;
      #  user = "cory";
      #  extraConfig = ''
      #    [greeter-theme]
      #    background-color = "#f0f0f0"
      #    text-color = "#0f0f0f"
      #    error-color = "#0f0f0f"
      #    password-color = "0f0f0f"
      #    password-background-color = "#f0f0f0"
      #    window-color = "#f0f0f0"
      #    border-color = "#0f0f0f"
      #  '';
      #};
    };
   
    windowManager.fvwm = {
      enable = true;
    };
  };

  # Emacs
  services.emacs.enable = true;
  systemd.user.services.emacs.unitConfig.ConditionGroup = "users"; # only start emacs for actual users
  environment.variables.EDITOR = "emacs -nw";

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  sound.mediaKeys.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;
  services.xserver.libinput.touchpad = {
    accelProfile = "adaptive";
    accelSpeed = null;
    additionalOptions = "";
    buttonMapping = null;
    calibrationMatrix = null;
    clickMethod = "clickfinger";
    dev = null;
    disableWhileTyping = true;
    horizontalScrolling = true;
    leftHanded = false;
    middleEmulation = false;
    naturalScrolling = true;
    scrollButton = null;
    scrollMethod = "twofinger";
    sendEventsMode = "enabled";
    tapping = false;
    tappingDragLock = false;
    transformationMatrix = "3 0 0 0 3 0 0 0 1";
  };
  services.xserver.libinput.mouse = {
    accelProfile = "flat";
    accelSpeed = null;
    disableWhileTyping = true;
  };

  # Ksh
  environment.shells = [ pkgs.ksh ];
  
  # User passwords must be set declaratively, fixes erase on boot errors
  users.mutableUsers = false;
  users.users.root.passwordFile = "/persist/secrets/root";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.cory = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "audio" "libvirtd" ];
    createHome = true;
    home = "/home/cory";
    shell = pkgs.ksh;
    passwordFile = "/persist/secrets/cory";
  };

  # Enable doas instead of sudo
  security.sudo.enable = false;
  security.doas.enable = true;
  security.doas.extraRules = [{
    users = [ "cory" ];
    keepEnv = true;
    persist = true;
  }];

  # Enable flakes
  nix = {
    package = pkgs.nixVersions.latest;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    useSandbox = true;
    gc = {
      automatic = true;
      dates = "*:0/10";
    };
  };
  systemd.services.nix-gc.unitConfig.ConditionACPower = true;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.system = "x86_64-linux";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    wget
    curl
    firefox
    git
    networkmanager
    networkmanagerapplet
    which
    fd
    htop
    killall
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
  system.stateVersion = "21.11"; # Did you read the comment?

}
