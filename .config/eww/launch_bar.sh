#!/bin/bash

## Start eww daemon
if [[ ! $(pidof eww) ]]; then
	eww daemon
	sleep 1
fi

if [[ -f ~/.is_desktop ]]; then
	eww open-many \
			leftBar   \
			rightBar
else
	eww open laptopBar
fi
