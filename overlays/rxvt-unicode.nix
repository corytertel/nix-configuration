{ pkgs, ... }:

final: prev: {
  rxvt-unicode-unwrapped-emoji = let
    desktop-file = pkgs.writeTextDir "share/applications/rxvt-unicode-client.desktop" ''
      [Desktop Entry]
      Categories=System;TerminalEmulator
      Comment=A clone of the well-known terminal emulator rxvt
      Exec=urxvtc
      GenericName=rxvt-unicode-client
      Icon=utilities-terminal
      Name=URxvt (Client)
      Type=Application
      Version=1.4
    '';
  in
    pkgs.symlinkJoin {
      name = "rxvt-unicode-unwrapped";
      version = prev.rxvt-unicode-unwrapped-emoji.version;
      paths = [
        desktop-file
        prev.rxvt-unicode-unwrapped-emoji
      ];
    };
}
