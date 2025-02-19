{ config, lib, pkgs, ...}:

{
  ### Framework manual optimizations ###

  # Fix TRRS headphones missing a mic
  # https://community.frame.work/t/headset-microphone-on-linux/12387/3
  boot.extraModprobeConfig = lib.mkIf (lib.versionOlder pkgs.linux.version "6.6.8") ''
    options snd-hda-intel model=dell-headset-multi
  '';

  # For fingerprint support
  # services.fprintd.enable = lib.mkDefault true;

  # Custom udev rules
  services.udev.extraRules = ''
    # Ethernet expansion card support
    ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="0bda", ATTR{idProduct}=="8156", ATTR{power/autosuspend}="20"
  '';

  # Needed for desktop environments to detect/manage display brightness
  hardware.sensor.iio.enable = lib.mkDefault true;

  # Gnome 40 introduced a new way of managing power, without tlp.
  # However, these 2 services clash when enabled simultaneously.
  # https://github.com/NixOS/nixos-hardware/issues/260
  services.tlp.enable = lib.mkDefault ((lib.versionOlder (lib.versions.majorMinor lib.version) "21.05")
                                       || !config.services.power-profiles-daemon.enable);

  boot.blacklistedKernelModules = lib.optionals (!config.hardware.enableRedistributableFirmware) [
    "ath3k"
  ];

  services.libinput.enable = lib.mkDefault true;

  # TODO research whether this is good with zfs or not
  services.fstrim.enable = lib.mkDefault true;

  # Kernel >=6.7
  boot.kernelParams =
    [ "amd_pstate=active" "amdgpu.sg_display=0" ]
    # Workaround for SuspendThenHibernate:
    # https://lore.kernel.org/linux-kernel/20231106162310.85711-1-mario.limonciello@amd.com/
    ++ lib.optionals (lib.versionOlder config.boot.kernelPackages.kernel.version "6.8")
      ["rtc_cmos.use_acpi_alarm=1"];

  # AMD has better battery life with PPD over TLP:
  # https://community.frame.work/t/responded-amd-7040-sleep-states/38101/13
  services.power-profiles-daemon.enable = lib.mkDefault true;

  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;

  # Enables the amd cpu scaling https://www.kernel.org/doc/html/latest/admin-guide/pm/amd-pstate.html
  # On recent AMD CPUs this can be more energy efficient.

  services.xserver.videoDrivers = lib.mkDefault [ "modesetting" ];

  # hardware.opengl = {
  #   driSupport = lib.mkDefault true;
  #   driSupport32Bit = lib.mkDefault true;
  # ;

  boot.initrd.kernelModules = [ "amdgpu" ];

  hardware.graphics.extraPackages = with pkgs.rocmPackages; [ clr clr.icd ];

  # Sets the kernel version to the latest kernel to make the usage of the iGPU possible if your kernel version is too old
  # Disables scatter/gather which was introduced with kernel version 6.2
  # It produces completely white or flashing screens when enabled while using the iGPU of Ryzen 7000-series CPUs (Raphael)

  # Fix unreadable tty under high dpi
  console = {
    packages = [ pkgs.terminus_font ];
    font = "ter-124n";
  };

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };

  # Use an equalizer to fix distortion in speakers
  # I am using philonmetal from https://github.com/ceiphr/ee-framework-presets for the speaker.
  # Need to run this as a user, because audio output is based on the running user.
  home-manager.users.cory = {
    services.easyeffects.enable = true;
    xdg.configFile."easyeffects/output/speaker.json".source =
      ./easyeffects/output/speaker.json;
    xdg.configFile."easyeffects/output/headphones.json".source =
      ./easyeffects/output/headphones.json;
    # xdg.configFile."easyeffects/autoload/output/speaker.json".source =
    #   ./easyeffects/autoload/output/speaker.json;
    # xdg.configFile."easyeffects/autoload/output/headphones.json".source =
    #   ./easyeffects/autoload/output/headphones.json;
  };

  # The Framework Laptop 13 has a TFT-LCD display, manufactured by BOE, that is uncalibrated
  # from factory, giving a suboptimal default sRGB coverage. It is thus recommended to apply an
  # ICC profile to have better color reproduction.
}
