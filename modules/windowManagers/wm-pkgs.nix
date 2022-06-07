{ pkgs }:
with pkgs;

# A list of applications that will be used universally by
# every standalone window manager
# Essentially everything here is what a desktop environment already
# has and won't need (like KDE does not need any of these)
[
  flameshot
  lxappearance
  polkit
  lxqt.lxqt-policykit
  xbrightness
  xscreensaver
  xfce.orage
  xorg.xkill
  pavucontrol
  networkmanagerapplet
]
