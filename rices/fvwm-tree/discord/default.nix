{ home-manager, ... }:

{
  home.file = {
    ".config/BetterDiscord/themes/cory-mountain.theme.css".text = builtins.readFile ./themes/cory-mountain.theme.css;
  };
}
