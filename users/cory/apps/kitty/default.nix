{ home-manager, ... }:

{
  home.file = {
    ".config/kitty/kitty.conf".text = builtins.readFile ./kitty.conf;
  };
}
