{ home-manager, ... }:

{
  home.file = {
    "manual_installs/layout_switch.sh".source = ./layout_switch.sh;
  };
}
