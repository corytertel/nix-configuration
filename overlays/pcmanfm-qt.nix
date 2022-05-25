{ pkgs, ... }:

final: prev: {
  pcmanfm-qt = let
    desktop-file = pkgs.writeTextDir "share/applications/user-home.desktop" ''
      [Desktop Entry]
      Type=Application
      Exec=pcmanfm-qt /home/cory
      Icon=user-home
      Name=Home
    '';
  in
    pkgs.symlinkJoin {
      name = "pcmanfm-qt";
      paths = [
        desktop-file
        prev.pcmanfm-qt
      ];
    };
}
