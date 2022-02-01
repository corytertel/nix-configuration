# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Nix
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
}

