{ pkgs, ... }:

let
  rofi-config = import ../../config/rofi/config.nix { inherit pkgs; };
in {
  home-manager.users.cory.programs.rofi = {
    enable = true;
    terminal = "${pkgs.rxvt-unicode}/bin/urxvtc";
    # theme = ../../config/rofi/rofi.rasi;
    theme = "${rofi-config}";
  };
}
