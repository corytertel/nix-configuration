{ ... }:

{
  home.file = {
    ".config/xfce4/terminal/terminalrc".text = builtins.readFile ./terminalrc;
  };
}
