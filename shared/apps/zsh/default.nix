{ home-manager, ... }:

{
  home.file = {
    ".zshrc".text = builtins.readFile ./zshrc;
  };
}
