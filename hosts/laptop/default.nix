{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../shared
    ../../profiles/kde/laptop.nix
  ];

  nix.buildCores = 6;

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
      "pcie_aspm.policy=performance"
      "mitigations=off"
      "nohibernate"
      "elevator=none"
    ];
  };

  networking = {
    hostId = "635976a6";
    interfaces.wlp0s20f3.useDHCP = true;
  };

  powerManagement = {
    cpuFreqGovernor = "powersave";
    enable = true;
    powertop.enable = true;
  };

  services = {
    ananicy = {
      enable = true;
      package = pkgs.ananicy-cpp;
    };
    tlp.enable = true;
    thermald.enable = true;
    gnome.gnome-keyring.enable = true;
    xserver = {
      displayManager.autoLogin = {
        enable = true;
        user = "cory";
      };
      libinput.touchpad = {
        accelProfile = "adaptive";
        accelSpeed = null;
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
        transformationMatrix = "2.5 0 0 0 2.5 0 0 0 1";
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

    # home.pointerCursor = {
    #   # name = "Numix-Cursor";
    #   # name = "Vanilla-DMZ-AA";
    #   name = "Oxygen_White";
    #   size = 48;
    #   gtk.enable = true;
    #   # package = pkgs.numix-cursor-theme;
    #   # package = pkgs.vanilla-dmz;
    #   package = pkgs.libsForQt5.oxygen;
    #   x11 = {
    #     enable = true;
    #     defaultCursor = "left_ptr";
    #   };
    # };

    home.packages = with pkgs; [
      zoom-us
    ];
  };
}
