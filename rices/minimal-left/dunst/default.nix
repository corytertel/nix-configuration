{ home-manager, ... }:

{
  home.file = {
    ".config/dunst/dunstrc".text = builtins.readFile ./dunstrc;
  };
}
