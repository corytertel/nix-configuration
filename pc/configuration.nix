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
  nix.buildCores = 4;

  boot = {
    kernelPackages = pkgs.linuxPackages_zen;
    #kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [
      "pcie_aspm.policy=performance"
      "mitigations=off"
      "intel_iommu=on"
      "iommu=pt"
      "nohibernate"
    ];
  };

  networking.interfaces = {
    #enp0s20u1.useDHCP = true;
    enp3s0.useDHCP = true;
    wlp6s0.useDHCP = true;
  };

  # Xserver
  services.xserver = {
    videoDrivers = [ "nvidia" ];
    libinput.touchpad = {
      # Trackball settings
      accelProfile = "adaptive";
      accelSpeed = null;
      disableWhileTyping = true;
      transformationMatrix = "2 0 0 0 2 0 0 0 1";
      dev = "/dev/input/by-id/usb-047d_Kensington_Expert_Mouse-event-mouse";
    };
  };

  environment.systemPackages = with pkgs; [

  ];
}

