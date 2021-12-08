{ home-manager, ... }:

{
  home.file = {
    ".config/BetterDiscord/themes/gruvbox.theme.css".text = builtins.readFile ./gruvbox.theme.css;
  };
}
