{ config, lib, pkgs, ... }:

{
  programs.cory.emacs = {
    enable = true;
    popup = false;
    fonts = {
      monospace = {
        name ="JuliaMono Nerd Font";
        size = 140;
      };
      variable = {
        name ="Liberation Serif";
        size = 150;
      };
    };
  };

  programs.cory.nushell.enable = true;

  theme = with pkgs; {
    font = {
      serif = {
        package = liberation_ttf;
        name = "Liberation Serif";
        size = 12;
      };
      sansSerif = {
        package = liberation_ttf;
        name = "Liberation Sans";
        size = 12;
      };
      monospace = {
        package = julia-mono-nerdfont;
        name = "JuliaMono Nerd Font";
        size = 10;
      };
    };
  };
}
