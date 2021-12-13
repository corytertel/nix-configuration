{ home-manager, ... }:

{
  home.file = {
    ".kshrc".text = builtins.readFile ./kshrc;
  };
}
