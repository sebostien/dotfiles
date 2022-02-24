#!/bin/bash

pactl get-sink-volume @DEFAULT_SINK@ | grep -o '[0-9]*%' | head -1