{ home-manager, ... }:

{
  home.file = {
    ".config/BetterDiscord/themes/mountain.theme.css".source = ./mountain.theme.css;
  };
}
