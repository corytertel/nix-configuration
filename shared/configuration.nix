# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      #./theme_breeze.nix
      #./blender.nix
    ];

  # Dual booting with GRUB
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.enable = true;
  boot.loader.grub.devices = ["nodev"];
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.useOSProber = true;

  # Splash screen
  #boot.plymouth.enable = true;
  #boot.plymouth.theme = "breeze";

  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Phoenix";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.networkmanager.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable the X11 windowing system.
  # Configure keymap in X11
  services.xserver = {
    exportConfiguration = true;
    enable = true;
    xkbModel = "microsoft";
    layout = "us,ru";
    xkbOptions = "grp:toggle"; # ralt toggle keyboard
    #xkbOptions = "grp:caps_toggle"; # caps toggle keyboard
    xkbVariant = "winkeys";
  };

  # Enable DWM
  #services.xserver.windowManager.dwm.enable = true; 

  # Enable the Plasma 5 Desktop Environment.
  #services.xserver.desktopManager.plasma5.enable = true;

  nixpkgs.overlays = [
    # Use custom DWM build
    (final: prev: {
      dwm = prev.dwm.overrideAttrs (old: { 
        src = pkgs.fetchFromGitHub {
          owner = "corytertel";
          repo = "dwm";
          rev = "c66265a89867d684200370f8ce88226551b19b44";
          sha256 = "Jw2mUT7ZmpTJbMLZvvXeCdj1OppVp2yE7mqJo4jKByw=";
        };
      });
    })

    # Discord
    (self: super: { 
      discord = super.discord.overrideAttrs (_: { 
        src = builtins.fetchTarball "https://discord.com/api/download?platform=linux&format=tar.gz"; 
      });
    })

    # EmacsGcc
    (import (builtins.fetchGit {
      url = "https://github.com/nix-community/emacs-overlay.git";
      ref = "master";
      #rev = "1f0315a2e2b9376c86bd71bda459cc237c1c30e2";
      #rev = "bfc8f6edcb7bcf3cf24e4a7199b3f6fed96aaecf";
      rev = "c75b7c047cc4635b0ecdedfd4ad78e1ac76e41c5";
    }))

    (final: prev: {
      ungoogled-chromium = prev.ungoogled-chromium.override {
        commandLineArgs = toString [
          # Ungoogled flags
          "--disable-search-engine-collection"
          "--extension-mime-request-handling=always-prompt-for-install"
          "--popups-to-tabs"
          "--show-avatar-button=incognito-and-guest"

          # Experimental features
          "--enable-features=${
            final.lib.concatStringsSep "," [
              "BackForwardCache:enable_same_site/true"
              "CopyLinkToText"
              "OverlayScrollbar"
              "TabHoverCardImages"
              "VaapiVideoDecoder"
            ]
          }"

          # Dark mode
          #"--force-dark-mode"

          # Performance
          "--enable-gpu-rasterization"
          "--enable-oop-rasterization"
          "--enable-zero-copy"
          "--ignore-gpu-blocklist"
        ];
      };
    })

    # ERROR: not working. gets an error with the makefile every build
    # TODO: create a custom derivation
    #(final: prev: {
    #  blender = prev.blender.overrideAttrs ( old: {
    #   #src = builtins.fetchTarball {
    #     # 2.83.9
    #     #url = "https://download.blender.org/release/Blender2.83/blender-2.83.9-linux64.tar.xz";
    #     # from blender
    #     # sha256 = "08b4bc330c66e1125c7d0461507ffa3f078f802f26b71fd51bcd854d94c274ef";
    #     # from nix
    #     #sha256 = "13qk31wsg3i5mdmfi165gk5ppgxb6q43yk55vnmql7m9lfxlcyn4";
    #   #};
    #    pname = "blender";
    #    #version = "2.83.5";
    #    #version = "2.83.9";
    #    version = "2.83.18";
    #    src = builtins.fetchurl {
    #      #url = "https://download.blender.org/source/blender-2.83.5.tar.xz";
    #      #sha256 = "0xyawly00a59hfdb6b7va84k5fhcv2mxnzd77vs22bzi9y7sap43";
    #      #url = "https://download.blender.org/source/blender-2.83.9.tar.xz";
    #      #sha256 = "106w9vi6z0gi2nbr73g8pm40w3wn7dkjcibzvvzbc786yrnzvkhb";
    #      url = "https://download.blender.org/source/blender-2.83.18.tar.xz";
    #      sha256 = "1jfm0fjssp0d00p6j040vs4x0ahi3d484fya7q6949cdysxd149p";
    #    };
    #  });
    #})
  ];

  # Emacs
  services.emacs.enable = true;
  #services.emacs.package = pkgs.emacsGcc;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  sound.mediaKeys.enable = true;

  services.xserver.libinput = {
    enable = true; # enable touchpad support
    touchpad = {
      accelProfile = "adaptive";
      #accelProfile = "flat";
      accelSpeed = null;
      additionalOptions = "";
      buttonMapping = null;
      calibrationMatrix = null;
      clickMethod = "clickfinger"; #sets trackpad to two-finger rightclick instead of button areas
      dev = null;
      disableWhileTyping = true;
      horizontalScrolling = true;
      leftHanded = false;
      middleEmulation = false;
      naturalScrolling = true;
      scrollButton = null;
      scrollMethod = "twofinger";
      sendEventsMode = "enabled";
      tapping = false;
      tappingDragLock = false;
      #transformationMatrix = "14 0 0 0 14 0 0 0 1";
      transformationMatrix = "3 0 0 0 3 0 0 0 1";
    };
    mouse = {
      # Mouse settings
      accelProfile = "flat";
      accelSpeed = null;
      disableWhileTyping = true;

      # Trackball settings
      #accelProfile = "adaptive";
      #accelSpeed = null;
      #disableWhileTyping = true;
      #transformationMatrix = "2 0 0 0 2 0 0 0 1";
    };
  };

  # Bash
  #programs.bash.enableCompletion = true;

  # Nushell
  #users.users.cory.shell = pkgs.nushell;

  # Ksh
  #programs.bash.enable = false;
  environment.shells = [ pkgs.ksh ];
  users.users.cory.shell = pkgs.ksh;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.cory = {
    isNormalUser = true;
    extraGroups = [ "wheel" "network" "audio" "libvirtd" ];
  };

  # Enable doas instead of sudo
  security.sudo.enable = false;
  security.doas.enable = true;
  security.doas.extraRules = [{
	  users = [ "cory" ];
	  keepEnv = true;
    persist = true;
  }];

  # Enable flakes
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [

   # install essentials
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    curl
    firefox
    git
    networkmanager
    networkmanagerapplet
    dialog
    wpa_supplicant
    mtools
    dosfstools
    parted
    gparted
   # base development tools 
    autoconf
    automake
    binutils
    bison
    fakeroot
    file
    findutils
    flex
    gawk
    gcc
    gettext
    gnugrep
    groff
    gzip
    libtool
    gnum4
    cmake
    gnumake
    pacman
    gnupatch
    pkgconf
    sedutil
    texinfo
    which
    fd
    coreutils
   # end base devel tools
    linuxHeaders
    avahi
    bluez
    cups
    pulseaudio
    pavucontrol
    pamixer
    alsaUtils
    os-prober
    ntfs3g
   # end install essentials

   # for window managers
    dunst

   # emacsGcc
    #emacsGcc

   # xmonad
    haskellPackages.xmobar
    xorg.xkill

    #blender

    nushell
    ksh
  ];

  # Fonts
  fonts.fonts = with pkgs; [
    #roboto
    roboto-mono
    #iosevka
    nerdfonts
    source-code-pro
    font-awesome
    dina-font
    cozette
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Virtual Box
  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "cory" ];

  # virt-manager
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;

  # Enable steam
  programs.steam.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 14d";
  };

}

