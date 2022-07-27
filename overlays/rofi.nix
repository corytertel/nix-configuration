{ pkgs, ... }:

final: prev: {
  rofi = let
    # Icon=kde
    desktop-file = pkgs.writeTextDir "share/applications/rofi.desktop" ''
      [Desktop Entry]
      Type=Application
      Exec=rofi -show drun -modi drun,run -show-icons
      Icon=system-search
      Name=Rofi Launcher
    '';
  in
    pkgs.symlinkJoin {
      name = "rofi";
      paths = [
        desktop-file
        prev.rofi
      ];
    };
}
