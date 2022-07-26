{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.programs.cory.krusader;
in {
  options.programs.cory.krusader = {
    enable = mkEnableOption "Enable krusader";
  };

  config = mkIf cfg.enable {
    home-manager.users.cory.home.packages = with pkgs; [
      krusader
      kdiff3 # or kompare
      krename
      thunderbird
      gnutar
      gzip
      bzip2
      xz
      unzip
      rar
      # unrar
      rpm
      dpkg
      arj
      lha
    ];

    apps.fileManager = {
      name = "krusader";
      command = "krusader";
      desktopFile = "org.kde.krusader.desktop";
      package = pkgs.krusader;
    };
  };
}
