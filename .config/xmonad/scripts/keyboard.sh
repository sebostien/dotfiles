#!/bin/sh


current=$(localectl | grep X11 | awk '{print $3}')

if [ "$current" != se ]; then
  # Disable page-up/down on laptop
  if [ -f "/home/sn/.is_desktop" ]; then
    xmodmap -e 'keycode 117='
    xmodmap -e 'keycode 112='
  fi
  localectl set-x11-keymap se
  localectl set-keymap se
  setxkbmap se -option caps:swapescape
  eww update locale="se"
else
  # Disable page-up/down on laptop
  if [ -f "/home/sn/.is_desktop" ]; then
    xmodmap -e 'keycode 117='
    xmodmap -e 'keycode 112='
  fi
  localectl set-x11-keymap us
  localectl set-keymap us
  setxkbmap us -option caps:swapescape
  eww update locale="us"
fi
