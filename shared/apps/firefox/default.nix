{ config, lib, pkgs, ... }:

{
  programs.firefox = {
    enable = true;
    package = pkgs.firefox.override {
      cfg.enableGnomeExtensions = true;
    };
    # profiles.cory = {
    #   bookmarks = {
    #     youtube.url = "https://www.youtube.com/";
    #     tiktok.url = "https://www.tiktok.com/";
    #     nixpkgs = "https://search.nixos.org/";
    #     home-manager = "https://nix-community.github.io/home-manager/options.html";
    #     myasu = "https://my.asu.edu";
    #     printing = "https://printanywhere.asu.edu/myprintcenter/";
    #     slashG.url = "https://boards.4channel.org/g/";
    #     unixporn = "https://www.reddit.com/r/unixporn";
    #   };
    #   isDefault = true;
    # };
  };
}
