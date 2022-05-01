{ pkgs, ... }:

{
  home.file.".config/tint2/tint2_vol.sh" = {
    executable = true;
    text = ''
#!/bin/sh

if [ "$1" == "up" ]; then
    amixer set Master 5%+ >/dev/null 2>&1
elif [ "$1" == "down" ]; then
    amixer set Master 5%- >/dev/null 2>&1
elif [ "$1" == "mute" ]; then
    a=$(amixer set Master 1+ toggle);
fi

vpattern=".*\[([0-9]+)%\].*"
spattern=".*\[off\].*"

amixer="amixer"

master=$($amixer sget 'Master')

vol=$(echo $master | awk -F"[][]" '/%/ { print $2 }' | head -n 1 | tr -d '%')

jackdev=$($amixer contents | grep -i "'headphone jack'" | cut -d"," -f1,2)

THEME="${pkgs.numix-icon-theme-circle}/share/icons/Numix-Circle"


if grep -qi $spattern <<< $master; then
    icon="🔇"
    ipath="$(find "$THEME" -name audio-volume-muted.png | grep 24 | head -n1)"
elif [ $vol -eq 0 ]; then
    icon="🔇"
    ipath="$(find "$THEME" -name audio-volume-muted.png | grep 24 | head -n1)"
elif grep -qi 'values=on' <<< $($amixer cget "$jackdev"); then
    icon="🎧"
    ipath="$(find "$THEME" -name *headphone* | grep 24 | head -n1)"
elif [ $vol -lt 31 ]; then
    icon="🔈"
    ipath="$(find "$THEME" -name audio-volume-low.png | grep 24 | head -n1)"
elif [ $vol -gt 30  ] && [ $vol -lt 60 ]; then
    icon="🔉"
    ipath="$(find "$THEME" -name audio-volume-medium.png | grep 24 | head -n1)"
else
    icon="🔊"
    ipath="$(find "$THEME" -name audio-volume-high.png | grep 24 | head -n1)"
fi

if [ -z $1 ]; then
    echo -e "$icon\n$vol%"
else
    notify-send -i $ipath "$vol%"
fi
    '';
  };
}
