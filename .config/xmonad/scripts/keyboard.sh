#!/bin/sh


if [ "$1" = "se" ]; then
  current="us"
elif [ "$1" = "us" ]; then
  current="se"
else
  current=$(localectl | grep X11 | awk '{print $3}')
fi

if [ "$current" != se ]; then
    localectl set-keymap se
    localectl set-x11-keymap se
    setxkbmap se -option caps:swapescape
    eww update locale="se"
    # Disable page-up/down on laptop
    if [ ! -e "/home/sn/.is_desktop" ]; then
        xmodmap -e 'keycode 117='
        xmodmap -e 'keycode 112='
    fi
else
    localectl set-keymap us
    localectl set-x11-keymap us
    setxkbmap us -option caps:swapescape
    eww update locale="us"
    # Disable page-up/down on laptop
    if [ ! -e "/home/sn/.is_desktop" ]; then
        xmodmap -e 'keycode 117='
        xmodmap -e 'keycode 112='
    fi
fi
