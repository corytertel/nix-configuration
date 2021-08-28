{ config, pkgs, ... }:

{
  imports =
    [
      ./apps/kitty
      ./apps/bash
    ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "cory";
  home.homeDirectory = "/home/cory";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  home.packages = with pkgs; [
    htop
  ];

  home.file = {
    #".config/kitty/kitty.conf".text = builtins.readFile ~/.nix-configuration/users/cory/apps/kitty/kitty.conf;
  };
}
