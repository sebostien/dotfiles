#!/bin/sh

# Read the _XMONAD_LOG property on root window
# xmonad sets the property to formatted yuck

# Remove property name
# Remove last "
# Replace \" with "

xprop -root -spy -notype _XMONAD_LOG | \
    stdbuf -oL sed -e 's/^_XMONAD_LOG = "//'  -e 's/.$//'   -e 's/\\"/"/g'
