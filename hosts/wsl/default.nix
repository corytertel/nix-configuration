{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    # "${modulesPath}/profiles/minimal.nix"
    # ./network-drive.nix
    ../../profiles/wsl
  ];

  wsl = {
    enable = true;
    defaultUser = "cory";
    wslConf.automount.root = "/mnt";
  };

  # WSL is closer to a container than anything else
  boot.isContainer = true;

  networking.hostName = "wsl";

  environment.etc.hosts.enable = false;
  environment.etc."resolv.conf".enable = false;

  networking.dhcpcd.enable = false;

  users.users = {
    root = {
      # Otherwise WSL fails to login as root with "initgroups failed 5"
      extraGroups = [ "root" ];
    };
    cory = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "network" "audio" "video" "libvirtd" ];
      home = "/home/cory";
    };
  };

  security.sudo.wheelNeedsPassword = false;

  # Disable systemd units that don't make sense on WSL
  systemd.services."serial-getty@ttyS0".enable = false;
  systemd.services."serial-getty@hvc0".enable = false;
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@".enable = false;

  systemd.services.firewall.enable = false;
  systemd.services.systemd-resolved.enable = false;
  systemd.services.systemd-udevd.enable = false;

  # Don't allow emergency mode, because we don't have a console.
  systemd.enableEmergencyMode = false;

  time.timeZone = "America/Phoenix";

  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true;

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    gc = {
      automatic = false;
    };
    optimise = {
      automatic = false;
    };
    settings = {
      auto-optimise-store = false;
      trusted-users = [ "root" "cory" ];
    };
  };

  system.activationScripts.binbash = {
    deps = [ "binsh" ];
    text = ''
      ln -sfn /bin/sh /bin/bash
    '';
  };

  fonts = with config.theme.font; {
    enableDefaultPackages = true;
    fontDir.enable = true;
    packages = with pkgs; [
      serif.package
      sansSerif.package
      monospace.package
    ];
    fontconfig = {
      # TODO figure out what sets this and remove it rather than use mkForce
      enable = lib.mkForce true;
      #  to share Windows fonts with WSL
      localConf = ''
        <dir>/mnt/c/Windows/Fonts</dir>
      '';
      defaultFonts = {
        serif = [ "${serif.name}" ];
        sansSerif = [ "${sansSerif.name}" ];
        monospace = [ "${monospace.name}" ];
      };
    };
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql;
    dataDir = "/postgresql/data";
    authentication = lib.mkForce ''
      # Generated file; do not edit!
      local all all              trust
      host  all all 127.0.0.1/32 md5
      host  all all ::1/128      md5
    '';
  };

  environment.systemPackages = with pkgs; [
    mg
    wget
    curl
    git
    gcc
    gnumake
    cmake
    which
    ripgrep
    trim-generations
  ];

  home-manager.users.cory = {
    programs = {
      home-manager.enable = true;
      nix-index.enable = true;
      git = {
        enable = true;
        userName  = "corytertel";
        userEmail = "ctertel@comcast.net";
      };
    };

    home = {
      username = "cory";
      homeDirectory = "/home/cory";

      # Make nix-shell work with nixpkgs from system flake
      # sessionVariables.NIX_PATH = "nixpkgs=${pkgs.outPath}";

      packages = with pkgs; [
        # nix
        nil

        # python
        jetbrains.pycharm-community
        pipreqs
        (python311.withPackages (ps: with ps; [
          epc
          python-lsp-server
          hy
          pygments
          pip
          python-dotenv
          # flask
          # flask-wtf
          # flask-sqlalchemy
          # flask_migrate
          # psycopg2
          # pandas
        ]))

        # web
        yarn
        nodejs
        nodePackages_latest.typescript-language-server

        # postgres
        postgresql
        postgresql_jdbc
        dbeaver

        # utils
        fd
        ripgrep
        yt-dlp
        unzip
        nix-prefetch-github
        imagemagick
      ];

      stateVersion = "23.11";
    };
  };

  system.stateVersion = "23.11";
}
