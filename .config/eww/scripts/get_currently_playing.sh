#!/bin/bash

get_data() {
  data=$(playerctl --player=$1 metadata --format "{{ artist }} - {{ title }}")
  echo $data
}

get_icon() {
  echo "<span foreground='$1' font_weight='bold'>$2</span>"
}

spotify=$(playerctl --player=spotify status 2> /dev/null)
if [ "$spotify" == "Playing" ]; then
  
  if [ "$1" == "icon" ]; then
    echo $(get_icon "#55cc33" "")
    exit
  fi
  
  echo $(get_data "spotify")
  exit;
fi

chrome=$(playerctl --player=chromium status 2> /dev/null)
if [ "$chrome" == "Playing" ]; then

  if [ "$1" == "icon" ]; then
    echo $(get_icon "#4285f4" "")
    exit
  fi

  echo $(get_data "chromium")
  exit;
fi

firefox=$(playerctl --player=firefox status 2> /dev/null)
if [ "$firefox" == "Playing" ]; then

  if [ "$1" == "icon" ]; then
    echo $(get_icon "#DE5825" "")
    exit
  fi

  echo $(get_data "firefox")
  exit;
fi

echo ""