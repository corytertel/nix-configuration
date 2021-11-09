{ home-manager, ... }:

{
  home.file = {
    ".bashrc".text = builtins.readFile ./bashrc2;
  };
}
