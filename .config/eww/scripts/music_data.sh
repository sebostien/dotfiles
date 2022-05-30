#!/bin/bash

get_percent() {
    echo $(playerctl -p playerctld metadata --format "{{ (position-0) / (mpris:length-0) }}")
}

# Download cover to /tmp/
get_cover() {


    # TODO: Only works with png :/
    url=$(playerctl -p playerctld metadata --format "{{ mpris:artUrl }}")

    if [[ -z $url ]]; then
        echo "defaultCover.png"
        exit
    fi

    file="/tmp/$(basename $url).png"

    # Keep local cache
    if [[ ! -f $file ]]; then
        notify-send "Download cover"
        wget --output-document $file $url
    fi

    echo $file
}

get_artist() {
    echo $(playerctl -p playerctld metadata --format "{{ artist }}")
}

get_title() {
    echo $(playerctl -p playerctld metadata --format "{{ title }}")
}

get_album() {
    echo $(playerctl -p playerctld metadata --format "{{ album }}")
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