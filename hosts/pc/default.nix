{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../shared
    ../../profiles/fvwm-pc
  ];

  nix.settings.cores = 4;

  boot = {
    # kernelPackages = pkgs.linuxPackages_5_15;
    kernelPackages = pkgs.linuxPackages_xanmod;
    kernelParams = [
      "pcie_aspm.policy=performance"
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

  console.font = "Lat2-Terminus16";

  programs = {
    gamemode.enable = true;
  };

  services = {
    xserver.videoDrivers = [ "nvidia" ];
  };

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      vaapiVdpau
    ];
  };

  environment.systemPackages = with pkgs; [
    xboxdrv
  ];

  home-manager.users.cory = {
    xresources.extraConfig = ''
      Xft.dpi: 150
      Xft.antialias: 1
      Xft.hinting: 1
      Xft.autohint: 0
      Xft.hintstyle: hintslight
      Xft.rgba: rgb
      Xft.lcdfilter: lcddefault
    '';

    home.packages = with pkgs; [
      airshipper
      steamPackages.steamcmd
      minecraft
    ];
  };
}
