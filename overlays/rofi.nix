{ pkgs, ... }:

final: prev: {
  rofi = let
    # Icon=system-search
    desktop-file = pkgs.writeTextDir "share/applications/rofi.desktop" ''
      [Desktop Entry]
      Type=Application
      Exec=rofi -show drun -modi drun,run -show-icons -scroll-method 0 -sort -hover-select -me-select-entry "" -me-accept-entry MousePrimary
      Icon=kde
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
