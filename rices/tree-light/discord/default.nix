{ home-manager, ... }:

{
  home.file = {
    ".config/BetterDiscord/themes/light-mountain.theme.css".text = builtins.readFile ./themes/light-mountain.theme.css;
  };
}
