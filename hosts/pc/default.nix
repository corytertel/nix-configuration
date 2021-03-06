{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../shared
    ../../profiles/kde/pc.nix
  ];

  nix.settings.cores = 4;

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

    # home.pointerCursor = {
    #   name = "Vanilla-DMZ-AA";
    #   size = 32;
    #   gtk.enable = true;
    #   package = pkgs.vanilla-dmz;
    #   x11 = {
    #     enable = true;
    #     defaultCursor = "left_ptr";
    #   };
    # };

    home.packages = with pkgs; [
      airshipper
      steamPackages.steamcmd
      minecraft
    ];
  };
}
