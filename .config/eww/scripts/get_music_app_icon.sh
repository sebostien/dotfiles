#!/bin/bash

# Really, Really bad script
# Must be a better way to do this

# TODO: use --follow
# playerctl does not like multiple paused players

declare -A icons
icons=([spotify]="" [chromium]="" [firefox]="")

# Grab the first player that is not paused
status=$(
    playerctl -a metadata --format "{{ playerName }} {{ status }}" \
  | grep Playing | awk '{print $1}'
  )

echo ${icons[$status]}
