{ home-manager, ... }:

{
  home.file = {
    ".config/dunst/dunstrc".text = builtins.readFile ./dunstrc;
    ".config/dunst/volume.sh".text = builtins.readFile ./volume.sh;
  };
}
