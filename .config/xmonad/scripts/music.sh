#!/bin/bash

# Used in xmobar

spotify=$(playerctl --player=spotify status)

if [ "$spotify" == "Playing" ]; then
  data=$(playerctl --player=spotify metadata --format "{{ artist }} - {{ title }}")
  echo -n "<fc=#55cc33><fn=1> </fn> $data </fc>"
  exit;
fi

firefox=$(playerctl --player=firefox status)

if [ "$firefox" == "Playing" ]; then
  data=$(playerctl --player=firefox metadata --format "{{ artist }} - {{ title }}")
  echo -n "<fc=#DE5825><fn=1> </fn> $data </fc>"
  exit;
fi

chrome=$(playerctl --player=chromium.instance382226 status)

if [ "$chrome" == "Playing" ]; then
  data=$(playerctl --player=chromium.instance382226 metadata --format "{{ artist }} - {{ title }}")
  echo -n "<fc=#4285f4><fn=1> </fn> $data </fc>"
  exit;
fi