#!/bin/sh

cat ~/.config/xmonad/xmonadKeys.txt | \
  yad \
      --text-info \
      --back=#282c34 \
      --fore=#46d9ff \
      --title=Keybindings \
      --undecorated \
      --margins=4 \
      --geometry=720x960