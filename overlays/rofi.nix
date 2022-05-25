{ pkgs, ... }:

final: prev: {
  rofi = let
    # wrapped = pkgs.writeShellScriptBin "hello" ''
    #   exec ${pkgs.hello}/bin/hello -t
    # '';
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
        # wrapped
        desktop-file
        prev.rofi
      ];
    };
}
