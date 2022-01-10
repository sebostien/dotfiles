#!/bin/bash

spotify=$(playerctl --player=spotify status)

if [ "$spotify" == "Paused" ]; then
  playerctl --player=spotify play;
  exit;
fi

playerctl play; # Play first player found
