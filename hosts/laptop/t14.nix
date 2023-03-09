{ config, lib, pkgs, ...}:

{
  ### T14 manual optimizations ###

  nix.settings.cores = 16;

  # For the Qualcomm NFA-725A (Device 1103) wireless network controller
  # See https://bugzilla.redhat.com/show_bug.cgi?id=2047878
  # boot.kernelPackages = lib.mkIf (lib.versionOlder pkgs.linux.version "5.16") pkgs.linuxPackages_latest;

  # For suspending to RAM to work, set Config -> Power -> Sleep State to "Linux" in EFI.
  # See https://wiki.archlinux.org/index.php/Lenovo_ThinkPad_X1_Carbon_(Gen_6)#Suspend_issues

  # Fingerprint sensor requires a firmware-update to work.

  # Force use of the thinkpad_acpi driver for backlight control.
  # This allows the backlight save/load systemd service to work.
  boot.kernelParams = [ "acpi_backlight=native" ];

  # Trackpoint
  hardware.trackpoint.enable = lib.mkDefault true;
  hardware.trackpoint.emulateWheel = lib.mkDefault config.hardware.trackpoint.enable;

  # Fingerprint reader: login and unlock with fingerprint (if you add one with `fprintd-enroll`)
  services.fprintd.enable = true;

  # Gnome 40 introduced a new way of managing power, without tlp.
  # However, these 2 services clash when enabled simultaneously.
  # https://github.com/NixOS/nixos-hardware/issues/260
  services.tlp.enable = lib.mkDefault ((lib.versionOlder (lib.versions.majorMinor lib.version) "21.05")
                                       || !config.services.power-profiles-daemon.enable);

  boot.blacklistedKernelModules = lib.optionals (!config.hardware.enableRedistributableFirmware) [
    "ath3k"
  ];

  services.xserver.libinput.enable = lib.mkDefault true;

  boot = {
    kernelModules = [ "acpi_call" ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
  };

  services.fstrim.enable = lib.mkDefault true;

  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;

  # # AMD GPU
  # options.hardware.amdgpu.loadInInitrd = lib.mkEnableOption (lib.mdDoc
  #   "loading `amdgpu` kernelModule at stage 1. (Add `amdgpu` to `boot.initrd.kernelModules`)"
  # ) // {
  #   default = true;
  # };

  # config = lib.mkMerge [
  #   {
  #     services.xserver.videoDrivers = lib.mkDefault [ "amdgpu" ];

  #     hardware.opengl.extraPackages = with pkgs; [
  #       rocm-opencl-icd
  #       rocm-opencl-runtime
  #       amdvlk
  #     ];

  #     hardware.opengl.extraPackages32 = with pkgs; [
  #       driversi686Linux.amdvlk
  #     ];

  #     hardware.opengl = {
  #       driSupport = lib.mkDefault true;
  #       driSupport32Bit = lib.mkDefault true;
  #     };

  #     environment.variables.AMD_VULKAN_ICD = lib.mkDefault "RADV";
  #   }
  #   (lib.mkIf config.hardware.amdgpu.loadInInitrd {
  #     boot.initrd.kernelModules = [ "amdgpu" ];
  #   })
  # ];

  # Fix unreadable tty under high dpi
  console = {
    packages = [ pkgs.terminus_font ];
    font = "ter-124n";
  };

  # Bluetooth
  hardware.bluetooth.enable = true;
}
