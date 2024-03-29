# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "rpool/local/root";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/9025-5FC6";
      fsType = "vfat";
    };

  fileSystems."/nix" =
    { device = "rpool/local/nix";
      fsType = "zfs";
    };

  fileSystems."/home" =
    { device = "rpool/safe/home";
      fsType = "zfs";
    };

  fileSystems."/persist" =
    { device = "rpool/safe/persist";
      fsType = "zfs";
    };

  fileSystems."/storage/personal" =
    { device = "storagepool/safe/personal";
      fsType = "zfs";
    };

  fileSystems."/storage/media" =
    { device = "storagepool/safe/media";
      fsType = "zfs";
    };

  fileSystems."/storage/misc" =
    { device = "storagepool/safe/misc";
      fsType = "zfs";
    };

  fileSystems."/storage/games" =
    { device = "storagepool/unsafe/games";
      fsType = "zfs";
    };

  fileSystems."/storage/unsafe" =
    { device = "storagepool/unsafe/misc";
      fsType = "zfs";
    };

  fileSystems."/backup/personal" =
    { device = "backuppool/safe/personal";
      fsType = "zfs";
    };

  fileSystems."/backup/media" =
    { device = "backuppool/safe/media";
      fsType = "zfs";
    };

  fileSystems."/backup/misc" =
    { device = "backuppool/safe/misc";
      fsType = "zfs";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/f011c833-42f5-4354-a6a7-e2ab34b45fd9"; }
    ];

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  # high-resolution display
  # hardware.video.hidpi.enable = lib.mkDefault true;
}
