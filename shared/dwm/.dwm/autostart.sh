# DWM Commands

# DWM Status
~/dwm-bar/dwm_bar.sh &

# Compositor
picom -f &

# Wallpaper
feh --bg-fill $HOME/Pictures/Wallpapers/95869.jpg
#nitrogen --restore &

# Touchpad config
xinput --set-prop "DLL096D:01 06CB:CDE6 Touchpad" "Synaptics Tap Time" 0
xinput --set-prop "DLL096D:01 06CB:CDE6 Touchpad" "Synaptics Palm Detection" 1

# Notification daemon
dunst &
