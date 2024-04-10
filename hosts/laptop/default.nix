{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./framework.nix
    ../shared
    ../../profiles/fvwm-laptop
  ];

  boot = {
    loader.grub = {
      enableCryptodisk = true;
    };

    initrd.luks.devices.root = {
      device = "/dev/disk/by-uuid/e4c8de9b-12b6-477c-bb3c-cb649ee5b010";
      preLVM = true;
    };

    kernelPackages = pkgs.linuxPackages_6_7;
    kernelParams = [
      # ZFS required flags
      "nohibernate"
      "elevator=none"
    ];

    plymouth.enable = true;
  };

  networking = {
    hostId = "e5e55245"; # for zfs
    interfaces.wlp1s0.useDHCP = true; # wifi
    interfaces.enp195s0f3u1.useDHCP = true; # ethernet
  };

  powerManagement = {
    # cpuFreqGovernor = "powersave";
    cpuFreqGovernor = "ondemand";
    enable = true;
    # powertop.enable = true;
  };

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
      xkb.layout = "us_dvorak_iso";
      libinput = {
        touchpad = {
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
        };
        mouse = {
          # Trackball
          # accelProfile = "flat";
          # accelSpeed = null;
          accelProfile = "adaptive";
          accelSpeed = "0.5";
          buttonMapping = "1 8 2 4 5 6 7 3 9";
          # buttonMapping = "1 2 8 4 5 6 7 3 9";
          disableWhileTyping = true;
          naturalScrolling = true;
          scrollButton = 3;
          # scrollButton = 2;
          scrollMethod = "button";
          # transformationMatrix = "2.5 0 0 0 2.5 0 0 0 1";
        };
      };
    };
  };

  system.stateVersion = "23.05";

  # Bluetooth
  services.blueman.enable = true;

  home-manager.users.cory = {
    # Xft.antialias: 1
    # Xft.hinting: 1
    # Xft.autohint: 0
    # Xft.hintstyle: hintslight
    # Xft.rgba: rgb
    # Xft.lcdfilter: lcddefault
    xresources.extraConfig = ''
      Xft.dpi: 125
    '';

    home.stateVersion = "23.05";
  };
}
