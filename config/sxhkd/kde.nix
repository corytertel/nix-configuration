{ config, pkgs }:

let
  # Prefix
  p = "control + period ;";
  # p = "control + z ;";

  kwinShortcut = shortcut:
    "qdbus org.kde.kglobalaccel /component/kwin invokeShortcut '${shortcut}'";

  spectacleShortcut = shortcut:
    "qdbus org.kde.kglobalaccel /component/org_kde_spectacle_desktop invokeShortcut '${shortcut}'";

  mediaShortcut = shortcut:
    "qdbus org.kde.kglobalaccel /component/mediacontrol invokeShortcut '${shortcut}'";

  audioShortcut = shortcut:
    "qdbus org.kde.kglobalaccel /component/kmix invokeShortcut '${shortcut}'";

  yakuakeShortcut = shortcut:
    "qdbus org.kde.kglobalaccel /component/yakuake invokeShortcut '${shortcut}'";

in with config.apps; {
  "${p} t" = terminal.command;
  "${p} control + t" = yakuakeShortcut "toggle-window-state";
  "${p} e" = editor.command;
  "${p} control + e" = "emacs";
  "${p} w" = browser.command;
  "${p} r" = fileManager.command;
  "${p} d" = "discord";
  "${p} space" = launcher.command;
  "Menu" = launcher.command;

  "${p} a ; a" = musicPlayer.command;
  "${p} a ; n" = mediaShortcut "nextmedia";
  "${p} a ; p" = mediaShortcut "previousmedia";
  "${p} a ; s" = mediaShortcut "stopmedia";
  "${p} a ; t" = mediaShortcut "playpausemedia";
  "F5" = audioShortcut "decrease_volume";
  "F6" = audioShortcut "increase_volume";
  "F7" = audioShortcut "mute";
  "F8" = mediaShortcut "playpausemedia";
  "F9" = mediaShortcut "previousmedia";
  "F10" = mediaShortcut "nextmedia";

  "${p} k" = kwinShortcut "Window Close";
  "${p} u" = kwinShortcut "Window Maximize";
  "${p} i" = kwinShortcut "Window Minimize";
  "${p} c" = kwinShortcut "Window Move Center";
  "${p} {1-5}" = kwinShortcut "Switch to Desktop {1-5}";
  # Doesn't work for some reason
  # "${p} control + {1-5}" = "${kwinShortcut "Window to Desktop {1-5}"} && ${kwinShortcut "Switch to Desktop {1-5}"}";
  "${p} control + 1" = "${kwinShortcut "Window to Desktop 1"} && ${kwinShortcut "Switch to Desktop 1"}";
  "${p} control + 2" = "${kwinShortcut "Window to Desktop 2"} && ${kwinShortcut "Switch to Desktop 2"}";
  "${p} control + 3" = "${kwinShortcut "Window to Desktop 3"} && ${kwinShortcut "Switch to Desktop 3"}";
  "${p} control + 4" = "${kwinShortcut "Window to Desktop 4"} && ${kwinShortcut "Switch to Desktop 4"}";
  "${p} control + 5" = "${kwinShortcut "Window to Desktop 5"} && ${kwinShortcut "Switch to Desktop 5"}";
  "${p} o" = kwinShortcut "Walk Through Windows (Reverse)";
  "${p} control + o" = kwinShortcut "Walk Through Windows";
  "${p} p" = kwinShortcut "Window On All Desktops";

  "${p} s" = spectacleShortcut "FullScreenScreenShot";
  "${p} control + s" = spectacleShortcut "RectangularRegionScreenShot";

  "control + comma" = kwinShortcut "Walk Through Windows (Reverse)";

  "control + button9" = kwinShortcut "Switch One Desktop Up";
  "control + button8" = kwinShortcut "Switch One Desktop Down";
  "control + button2" = kwinShortcut "Window Move Center";
  "alt + button9" = kwinShortcut "ExposeAll";
  "alt + button8" = kwinShortcut "Show Desktop";
  "button9" = kwinShortcut "Walk Through Windows (Reverse)";
  "button8" = kwinShortcut "Walk Through Windows";

  "control + alt + space" = "${pkgs.layout-switch}/bin/layout-switch";

  # "${p} control + z" = "${pkgs.xdotool}/bin/xdotool key control+z";
}
