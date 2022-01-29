{ home-manager, ... }:

{
  home.file = {
    ".config/BetterDiscord/themes/blacknord.theme.css".text = builtins.readFile ./blacknord.theme.css;
  };
}
