{ config, lib, pkgs, ... }:

{
  ### XPS 9300 manual optimizations ###

  nix.settings.cores = 8;

  # intel integrated gpu
  boot.initrd.kernelModules = [ "i915" ];

  environment.variables = {
    VDPAU_DRIVER = lib.mkIf config.hardware.opengl.enable (lib.mkDefault "va_gl");
  };

  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    libvdpau-va-gl
    intel-media-driver
  ];

  # general laptop

  boot.blacklistedKernelModules = (lib.optionals (!config.hardware.enableRedistributableFirmware) [
    "ath3k"
  ])
  ++
  # The touchpad uses I²C, so PS/2 is unnecessary
  # Without this we get errors in dmesg on boot and hangs when shutting down.
  [ "psmouse" ];

  services.xserver.libinput.enable = lib.mkDefault true;

  # Gnome 40 introduced a new way of managing power, without tlp.
  # However, these 2 services clash when enabled simultaneously.
  # https://github.com/NixOS/nixos-hardware/issues/260
  services.tlp.enable = lib.mkDefault
    ((lib.versionOlder (lib.versions.majorMinor lib.version) "21.05")
     || !config.services.power-profiles-daemon.enable);

  # ssd
  # unknown whether to use with zfs or not
  # services.fstrim.enable = lib.mkDefault true;

  # Includes the Wi-Fi and Bluetooth firmware for the QCA6390.
  hardware.enableRedistributableFirmware = true;

  # Allows for updating firmware via `fwupdmgr`.
  services.fwupd.enable = true;

  # This will save you money and possibly your life!
  services.thermald.enable = lib.mkDefault true;

  # prevent early throttling
  services.throttled.enable = lib.mkDefault true;

  # helps tlp to work
  boot = {
    kernelModules = [ "acpi_call" ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
  };

  # Fix unreadable tty under high dpi
  console = {
    packages = [ pkgs.terminus_font ];
    font = "ter-132n";
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
    # cpuFreqGovernor = "performance";
    cpufreq.min = 1300000;
  };
}
