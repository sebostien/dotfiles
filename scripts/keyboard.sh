#!/usr/bin/env bash


if [ "$1" = "se" ]; then
  current="us"
elif [ "$1" = "us" ]; then
  current="se"
else
  current=$(setxkbmap -query | grep layout | awk '{ print $2 }')
fi

if [ "$current" != se ]; then
    setxkbmap se -option caps:swapescape
    eww update locale="se"
    # Disable page-up/down on laptop
    if [ ! -e "/home/sn/.is_desktop" ]; then
        xmodmap -e 'keycode 117='
        xmodmap -e 'keycode 112='
    fi
else
    setxkbmap us -option caps:swapescape
    eww update locale="us"
    # Disable page-up/down on laptop
    if [ ! -e "/home/sn/.is_desktop" ]; then
        xmodmap -e 'keycode 117='
        xmodmap -e 'keycode 112='
    fi
fi
