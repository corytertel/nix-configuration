{ home-manager, ... }:

{
  home.file = {
    ".spacemacs".text = builtins.readFile ./spacemacs;
  };
}
