{ ... }:

{
  home.file = {
    ".config/pcmanfm/default/pcmanfm.conf".text = builtins.readFile ./pcmanfm.conf;
  };
}
