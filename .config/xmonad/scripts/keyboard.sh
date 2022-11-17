#!/bin/sh


current=$(localectl | grep X11 | awk '{print $3}')

if [ "$current" != se ]; then
  gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'se')]"
  localectl set-x11-keymap se
  localectl set-keymap se
  notify-send "Changed keymap to 'se'" 
else
  gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us')]"
  localectl set-x11-keymap us
  localectl set-keymap us
  notify-send "Changed keymap to 'us'"
fi
