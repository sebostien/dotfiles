#!/bin/sh
status=$(nordvpn status)
connected=$(echo "$status" | grep Status | awk '{print $4}')

case "$connected" in
    *"Connected"*)
        country=$(echo "$status" | grep Country | awk '{print $2}')
        echo "#90c861 $country"
        # country=$(nordvpn status | grep Country)
        # ip=$(nordvpn status | grep Server)
        # echo -n "<fc=#4687ff><fn=1>嬨 </fn> $country - $ip </fc>  <fc=#666666>|</fc>"
    ;;
    *"Disconnected"*)
        echo "Disconnected"
    ;;
esac