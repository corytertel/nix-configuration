{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../shared
    ../../modules/window-managers/fvwm-pc
  ];

  nix.buildCores = 4;

  boot = {
    # kernelPackages = pkgs.linuxPackages_5_15;
    kernelPackages = pkgs.linuxPackages_xanmod;
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

  programs = {
    gamemode.enable = true;
  };

  services = {
    xserver.videoDrivers = [ "nvidia" ];
  };

  environment.systemPackages = with pkgs; [
    xboxdrv
  ];
}
