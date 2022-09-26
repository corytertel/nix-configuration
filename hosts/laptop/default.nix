{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./xps9300.nix
    ../shared
    ../../profiles/fvwm-laptop
  ];

  nix.settings.cores = 6;

  boot = {
    loader.grub = {
      version = 2;
      enableCryptodisk = true;
    };

    initrd.luks.devices.root = {
      device = "/dev/disk/by-uuid/15108fb0-8960-4581-8424-cefe6c550c90";
      preLVM = true;
    };

    #kernelPackages = pkgs.linuxPackages_latest;
    kernelPackages = pkgs.linuxPackages_5_15;
    kernelParams = [
      # ZFS required flags
      "nohibernate"
      "elevator=none"
    ];
  };

  networking = {
    hostId = "635976a6"; # for zfs
    interfaces.wlp0s20f3.useDHCP = true;
  };

  # powerManagement = {
  #   cpuFreqGovernor = "powersave";
  #   enable = true;
  #   powertop.enable = true;
  # };

  services = {
    # ananicy = {
    #   enable = true;
    #   package = pkgs.ananicy-cpp;
    # };
    # tlp.enable = true;
    # thermald.enable = true;
    # gnome.gnome-keyring.enable = true;
    xserver = {
      displayManager.autoLogin = {
        enable = true;
        user = "cory";
      };
      libinput.touchpad = {
        accelProfile = "adaptive";
        accelSpeed = "0.5";
        additionalOptions = "";
        buttonMapping = null;
        calibrationMatrix = null;
        clickMethod = "clickfinger"; #sets trackpad to two-finger rightclick instead of button areas
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
        # transformationMatrix = "2.5 0 0 0 2.5 0 0 0 1";
      };
    };
  };

  environment.systemPackages = with pkgs; [

  ];

  home-manager.users.cory = {
    xresources.extraConfig = ''
    Xft.dpi: 225
    Xft.antialias: 1
    Xft.hinting: 1
    Xft.autohint: 0
    Xft.hintstyle: hintslight
    Xft.rgba: rgb
    Xft.lcdfilter: lcddefault
  '';

    home.packages = with pkgs; [
      zoom-us
    ];
  };
}
