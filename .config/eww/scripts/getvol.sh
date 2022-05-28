#!/bin/bash

# Check mute
a=$(pactl get-sink-mute @DEFAULT_SINK@)

case "$a" in
    *"Mute: no"*)
        # vol=$(pactl get-sink-volume @DEFAULT_SINK@ | grep -o '[0-9]*%' | head -1)
        vol=$(amixer -D pulse get Master | grep -o '[0-9]*%' | head -1)
        echo $vol
    ;;
    *"Mute: yes"*)
        echo "muted"
    ;;
esac



