#!/bin/bash

## Start eww daemon
if [[ ! `pidof eww` ]]; then
	eww daemon
	sleep 1
fi

eww open-many \
    leftBar \
	rightBar