{ config, lib, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../shared
    ../../profiles/fvwm-pc
  ];

  nix.settings.cores = 4;

  boot = {
    kernelPackages = pkgs.linuxPackages_6_7;
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

  services.xserver = {
    videoDrivers = [ "nvidia" ];
    xkb.layout = "us_dvorak";
    libinput.mouse = {
      # Mouse
      # accelProfile = "flat";
      # accelSpeed = null;
      # disableWhileTyping = true;

      # Trackball
      # accelProfile = "flat";
      # accelSpeed = null;
      accelProfile = "adaptive";
      # accelSpeed = "0.5";
      accelSpeed = "0.7";
      buttonMapping = "1 8 2 4 5 6 7 3 9";
      # buttonMapping = "1 2 8 4 5 6 7 3 9";
      disableWhileTyping = true;
      naturalScrolling = true;
      scrollButton = 3;
      # scrollButton = 2;
      scrollMethod = "button";
      # transformationMatrix = "5 0 0 0 5 0 0 0 1";
    };
  };

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiVdpau
    ];
  };

  environment.systemPackages = with pkgs; [
    xboxdrv
    vulkan-loader
  ];

  # environment.variables = {
  #   GDK_SCALE = "2";
  # };

  system.stateVersion = "21.11";

  home-manager.users.cory = {
    home.stateVersion = "21.11";
    xresources.extraConfig = ''
      Xft.dpi: 144
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
