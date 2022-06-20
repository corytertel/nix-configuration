let
  # Prefix
  p = "control + period ;";

  kwinShortcut = shortcut:
    "qdbus org.kde.kglobalaccel /component/kwin invokeShortcut '${shortcut}'";

  spectacleShortcut = shortcut:
    "qdbus org.kde.kglobalaccel /component/org_kde_spectacle_desktop invokeShortcut '${shortcut}'";

  mediaShortcut = shortcut:
    "qdbus org.kde.kglobalaccel /component/mediacontrol invokeShortcut '${shortcut}'";
in {
    "${p} t" = "urxvtc";
    "${p} e" = "emacsclient -c";
    "${p} w" = "firefox";
    "${p} r" = "pcmanfm-qt --new-window";
    "${p} d" = "discord";
    "${p} space" = "rofi -show drun -modi drun,run -show-icons";

    "${p} a ; a" = "audacious";
    "${p} a ; n" = mediaShortcut "nextmedia";
    "${p} a ; p" = mediaShortcut "previousmedia";
    "${p} a ; s" = mediaShortcut "stopmedia";
    "${p} a ; t" = mediaShortcut "playpausemedia";

    "${p} k" = kwinShortcut "Window Close";
    "${p} u" = kwinShortcut "Window Maximize";
    "${p} i" = kwinShortcut "Window Minimize";
    "${p} c" = kwinShortcut "Window Move Center";
    "${p} {1-5}" = kwinShortcut "Switch to Desktop {1-5}";
    "${p} control + {1-5}" = kwinShortcut "Window to Desktop {1-5}";
    "${p} o" = kwinShortcut "Walk Through Windows (Reverse)";
    "${p} control + o" = kwinShortcut "Walk Through Windows";

    "${p} s" = spectacleShortcut "FullScreenScreenShot";
    "${p} control + s" = spectacleShortcut "RectangularRegionScreenShot";

    "meta + space" = "layout-switch";
}
