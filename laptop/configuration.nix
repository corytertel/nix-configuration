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
  nix.buildCores = 6;

  boot = {
    kernelPackages = pkgs.linuxPackages_zen;
    kernelParams = [
      "pcie_aspm.policy=performance"
      "mitigations=off"
      "nohibernate"
    ];
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
    xserver.libinput.touchpad = {
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

  networking.interfaces.wlp0s20f3.useDHCP = true;

  environment.systemPackages = with pkgs; [

  ];
}

