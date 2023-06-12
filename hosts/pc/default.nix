{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../shared
    ../../profiles/xmonad-pc
  ];

  nix.settings.cores = 4;

  boot = {
    kernelPackages = pkgs.linuxPackages_6_1;
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
    xserver.layout = "us_qwerty";
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

  # environment.variables = {
  #   GDK_SCALE = "2";
  # };

  system.stateVersion = "21.11";

  home-manager.users.cory = {
    home.stateVersion = "21.11";
    xresources.extraConfig = ''
      Xft.dpi: 128
      Xft.autohint: 0
      Xft.lcdfilter: lcddefault
      Xft.hintstyle: hintfull
      Xft.hinting: 1
      Xft.antialias: 1
      Xft.rgba: rgb
    '';

    home.packages = with pkgs; [
      airshipper
      steamPackages.steamcmd
      minecraft
    ];
  };
}
