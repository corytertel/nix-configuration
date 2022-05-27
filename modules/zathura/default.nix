{ pkgs, ... }:

{
  home-manager.users.cory.home.file = {
    ".config/zathura/zathurarc".text = builtins.readFile ./zathurarc;
  };
}
