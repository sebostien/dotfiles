#!/bin/sh

current=$(localectl | grep X11 | awk '{print $3}')
echo $current
if [ "$current" != se ]; then
    setxkbmap se -option caps:swapescape
    localectl set-keymap se
    localectl set-x11-keymap se
    eww update locale="se"
    # Disable page-up/down on laptop
    if [ ! -e "/home/sn/.is_desktop" ]; then
        xmodmap -e 'keycode 117='
        xmodmap -e 'keycode 112='
    fi
else
    setxkbmap us -option caps:swapescape
    localectl set-x11-keymap us
    localectl set-keymap us
    eww update locale="us"
    # Disable page-up/down on laptop
    if [ ! -e "/home/sn/.is_desktop" ]; then
        xmodmap -e 'keycode 117='
        xmodmap -e 'keycode 112='
    fi
fi
