#!/bin/bash

spotify=$(playerctl --player=spotify status)

if [ "$spotify" == "Playing" ]; then
  playerctl --player=spotify next;
  exit;
fi

playerctl next; # next for the first player found