{ pkgs, ... }:

{
  home.file = {
    ".config/nu/config.toml".text = builtins.readFile ./config.toml;
  };
}
