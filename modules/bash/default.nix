{ home-manager, ... }:

{
  home-manager.users.cory.home.file = {
    ".bashrc".text = builtins.readFile ./bashrc;
  };
}
