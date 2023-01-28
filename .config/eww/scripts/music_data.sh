#!/bin/bash

get_percent() {
    playerctl -p playerctld metadata --format "{{ (position-0) / (mpris:length-0) }}"
}

# Download cover to /tmp/
get_cover() {
  url=$(playerctl -p playerctld metadata --format "{{ mpris:artUrl }}")

  if [[ -z $url ]]; then
    echo false
    exit
  fi
  
  # File schema
  if [[ "$url" == "file:///"* ]]; then
    file=$url
  else
    # TODO: Only works with png (I'm only using spotify so this works)
    file="/home/sn/.config/eww/album_covers/$(basename "$url").png"
    # Keep local cache
    if [[ ! -f $file ]]; then
        # Add song to CSV
        playerctl -p playerctld metadata --format "{{ mpris:artUrl }};|;{{ artist }};|;{{ album }};|;{{title}}" | sed 's:.*/::' >> /home/sn/.config/eww/album_covers/list.txt
        wget --output-document "$file" "$url"
    fi
  fi

  echo "$file"
}

get_artist() {
  playerctl -p playerctld metadata --format "{{ artist }}"
}

get_title() {
  playerctl -p playerctld metadata --format "{{ title }}"
}

get_album() {
  playerctl -p playerctld metadata --format "{{ album }}"
}

if [[ "$1" == "percent" ]]; then
    get_percent    
elif [[ "$1" == "cover" ]]; then
    get_cover
elif [[ "$1" == "artist" ]]; then
    get_artist
elif [[ "$1" == "title" ]]; then
    get_title
elif [[ "$1" == "album" ]]; then
    get_album
fi
