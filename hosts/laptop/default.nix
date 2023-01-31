{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./t14.nix
    ../shared
    ../../profiles/fvwm-laptop
  ];

  boot = {
    loader.grub = {
      version = 2;
      enableCryptodisk = true;
    };

    initrd.luks.devices.root = {
      device = "/dev/disk/by-uuid/e4c8de9b-12b6-477c-bb3c-cb649ee5b010";
      preLVM = true;
    };

    kernelPackages = pkgs.linuxPackages_6_0;
    kernelParams = [
      # ZFS required flags
      "nohibernate"
      "elevator=none"
    ];

    plymouth.enable = true;
  };

  networking = {
    hostId = "e5e55245"; # for zfs
    interfaces.wlp2s0.useDHCP = true; # wifi
    interfaces.enp1s0f0.useDHCP = true; # ethernet
  };

  # powerManagement = {
  #   # cpuFreqGovernor = "powersave";
  #   cpuFreqGovernor = "ondemand";
  #   enable = true;
  #   powertop.enable = true;
  # };

  services = {
    # ananicy = {
    #   enable = true;
    #   package = pkgs.ananicy-cpp;
    # };
    tlp.enable = true;
    # thermald.enable = true;
    # gnome.gnome-keyring.enable = true;
    xserver = {
      displayManager.autoLogin = {
        enable = true;
        user = "cory";
      };
      layout = "us_beakl_emacs";
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
      };
    };
  };

  system.stateVersion = "23.05";

  environment.systemPackages = with pkgs; [

  ];

  # home-manager.users.cory.services.xcape = {
  #   enable = true;
  #   mapExpression = {
  #     # Shift_L = "Escape";
  #     # Shift_R = "Escape";
  #   };
  #   timeout = 300;
  # };

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

    home.packages = with pkgs; [];

    home.stateVersion = "23.05";
  };
}
