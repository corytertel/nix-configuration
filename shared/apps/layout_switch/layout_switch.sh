#!/bin/sh

# If an explicit layout is provided as an argument, use it. Otherwise, select the next layout from
# the set [us, it, fr].
if [[ -n "$1" ]]; then
    setxkbmap $1
else
    layout=$(setxkbmap -query | grep layout | awk 'END{print $2}')
    case $layout in
        ru)
            setxkbmap us
            ;;
        *)
            setxkbmap ru
            ;;
    esac
fi
