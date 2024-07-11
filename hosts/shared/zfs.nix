{ config, lib, pkgs, ... }:

{
  # Zfs
  boot.supportedFilesystems = [ "zfs" ];
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/local/root@blank
  '';
  boot.zfs.package = pkgs.zfs_unstable;
  boot.loader.grub.copyKernels = true;

  #services.zfs.autoScrub.enable = true;
  #services.zfs.autoScrub.interval = "weekly";
  #systemd.services.zfs-scrub.unitConfig.ConditionACPower = true;

  # Erase on every boot fixes
  environment.etc = {
    "NetworkManager/system-connections".source = "/persist/etc/NetworkManager/system-connections/";
    "nixos".source = "/persist/etc/nixos/";
    "mullvad-vpn.key".source = "/persist/etc/mullvad-vpn.key";
  };

  users = {
    mutableUsers = false;
    users = {
      root.hashedPasswordFile = "/persist/secrets/root";
      cory = {
        createHome = true;
        hashedPasswordFile = "/persist/secrets/cory";
      };
    };
    extraGroups.vboxusers.members = [ "cory" ];
  };


  security = {
    sudo = {
      enable = true;
      extraConfig = "Defaults lecture=never";
    };
  };

  fileSystems."/persist".neededForBoot = true;
}
