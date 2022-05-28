#!/bin/bash

if [ $1 == true ]; then
    nordvpn connect
else
    nordvpn disconnect
fi

sleep 4

status=$(./scripts/getvpnstatus.sh)
eww update vpn="$status"
