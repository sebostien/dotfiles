#!/bin/bash

spotify=$(playerctl --player=spotify status)

if [ "$spotify" == "Playing" ]; then
  playerctl --player=spotify previous;
  exit;
fi

playerctl previous; # previous for the first player found
