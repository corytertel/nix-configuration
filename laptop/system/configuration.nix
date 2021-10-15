# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Libre Kernel
  #boot.kernelPackages = pkgs.linuxPackages_latest-libre;
  boot.kernelPackages = pkgs.linuxPackages-libre;

  # Zen Kernel
  #boot.kernelPackages = pkgs.linuxPackages_zen;
  #boot.kernelParams = [ "pcie_aspm.policy=performance" ];

  # Power
  powerManagement.cpuFreqGovernor = "powersave";
  powerManagement.enable = true;
  powerManagement.powertop.enable = true;
  services.tlp.enable = true;
  services.thermald.enable = true;
  #services.undervolt = {
  #  enable = true;
  #  coreOffset = -50;
  #};

  # Enable Xmonad
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad/xmonad.hs;
  };

  # Network Devices
  networking.interfaces.wlp0s20f3.useDHCP = true;

  environment.systemPackages = with pkgs; [

  ];
}

