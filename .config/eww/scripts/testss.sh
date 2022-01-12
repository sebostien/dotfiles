#!/bin/sh

xprop -root -notype _XMONAD_LOG | sed -e 's/^_XMONAD_LOG = "//' -e 's/.$//'