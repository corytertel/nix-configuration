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

  # Latest Kernel
  #boot.kernelPackages = pkgs.linuxPackages_latest;

  # Zen Kernel
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.kernelParams = [ "pcie_aspm.policy=performance" "mitigations=off" ];

  # Network Devices
  networking.interfaces.enp0s20u1.useDHCP = true;
  networking.interfaces.enp3s0.useDHCP = true;
  networking.interfaces.wlp6s0.useDHCP = true;

  # Xserver
  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };

  # Allow for Unfree Packages
  nixpkgs.config.allowUnfree = true;

  # Enable steam
  programs.steam.enable = true;

  environment.systemPackages = with pkgs; [

  ];
}

