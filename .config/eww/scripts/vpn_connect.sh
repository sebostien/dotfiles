#!/bin/bash

if [ $1 == true ]; then
    nordvpn connect
else
    nordvpn disconnect
fi
