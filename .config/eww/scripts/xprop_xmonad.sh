#!/bin/bash

# Read the _XMONAD_LOG property on root window
# xmonad sets the property to formatted markup for workspaces
#   and plain text for current window title.
# They are seperated by "|||" so we can split them apart

# Remove property name
# Remove last "
# Replace \" with "


if [ ! "$1" ]
then
    echo "ERROR: No argumnet for body or title"
fi

case "$1" in
  *"body"*)
    xprop -root -spy -notype _XMONAD_LOG | \
        stdbuf -oL sed -e 's/^_XMONAD_LOG = "//'  -e 's/|||.*//'   -e 's/\\"/"/g'
  ;;
  *"title"*)
    xprop -root -spy -notype _XMONAD_LOG | \
        stdbuf -oL sed -e 's/.*|||//' -e 's/.$//'
  ;;
esac


