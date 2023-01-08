#!/bin/sh


current=$(localectl | grep X11 | awk '{print $3}')

if [ "$current" != se ]; then
  localectl set-x11-keymap se
  localectl set-keymap se
  setxkbmap se
  # notify-send "Changed keymap to 'se'" 
  echo "se" > ~/.config/xmonad/scripts/locale
else
  localectl set-x11-keymap us
  localectl set-keymap us
  setxkbmap us
  # notify-send "Changed keymap to 'us'"
  echo "us" > ~/.config/xmonad/scripts/locale
fi
