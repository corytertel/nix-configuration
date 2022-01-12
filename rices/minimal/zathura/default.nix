{ home-manager, ... }:

{
  home.file = {
    ".config/zathura/zathurarc".text = builtins.readFile ./zathurarc;
  };
}
