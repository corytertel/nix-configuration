{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.programs.photogimp;
in {
  options.programs.photogimp = {
    enable = mkEnableOption "Enable photogimp";
  };

  config = mkIf cfg.enable {
    environment = {
      variables.GIMP2_DIRECTORY = "${pkgs.photogimp_config}";
      systemPackages = [
        pkgs.photogimp_gimpPatched
        pkgs.photogimp_config
        pkgs.photogimp_icon
      ];
    };
  };
}
