{ home-manager, ... }:

{
  home.file = {
    "manual_installs/layout_switch.sh".text = builtins.readFile ./layout_switch.sh;
  };
}
