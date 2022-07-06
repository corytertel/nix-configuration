{ pkgs, ... }:

# Yes this should be in an overlay for libsForQt5.plasma-workspace
# but I don't want to recompile plasma-workspace

final: prev: {
  krunner-desktop = pkgs.writeTextDir "share/applications/krunner.desktop" ''
      [Desktop Entry]
      Type=Application
      Exec=krunner
      Icon=kde
      Name=KRunner
    '';
}
