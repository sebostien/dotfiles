#!/bin/bash

## Start eww daemon
if [[ ! $(pidof eww) ]]; then
	eww daemon
	sleep 1
fi

if [[ -f ~/.is_laptop ]]; then
	eww open laptopBar
else
	eww open-many \
		leftBar \
		rightBar
fi
