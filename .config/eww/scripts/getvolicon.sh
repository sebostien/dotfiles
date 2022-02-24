#!/bin/bash

a=$(pactl get-sink-mute @DEFAULT_SINK@)

case "$a" in
    *"Mute: no"*)
        echo "墳"
    ;;
    *"Mute: yes"*)
        echo "婢"
    ;;
esac
