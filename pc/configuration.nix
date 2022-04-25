{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Nix
  nix.buildCores = 4;

  boot = {
    #kernelPackages = pkgs.linuxPackages_latest;
    kernelPackages = pkgs.linuxPackages_5_15;
    kernelParams = [
      "pcie_aspm.policy=performance"
      "mitigations=off"
      "intel_iommu=on"
      "iommu=pt"
      "nohibernate"
    ];
  };

  networking = {
    hostId = "271e42cf";
    interfaces = {
      # enp0s20u1.useDHCP = true;
      enp3s0.useDHCP = true;
      wlp6s0.useDHCP = true;
    };
  };

  services = {
    xserver.videoDrivers = [ "nvidia" ];
  };

  environment.systemPackages = with pkgs; [

  ];
}
