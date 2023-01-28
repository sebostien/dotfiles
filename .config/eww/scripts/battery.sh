#!/bin/bash

battery=/sys/class/power_supply/BAT0/

# TODO: Listen for change instead
[ "$(cat "$battery/status")" = Charging ] && echo "" && exit

charge="$(cat "$battery/capacity")"
icons=(          )
echo "${icons[$charge / 10]}"
