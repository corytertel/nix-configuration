{ home-manager, ... }:

{
  home.file = {
    ".config/BetterDiscord/themes/light-mountain.theme.css".text = builtins.readFile ./light-mountain.theme.css;
  };
}
