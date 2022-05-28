#!/bin/bash
status=$(nordvpn status)
connected=$(echo "$status" | grep Status)

case "$connected" in
    *"Status: Connected"*)
        country=$(echo "$status" | grep Country | awk '{print $2}')
        ip=$(echo "$status" | grep 'Server IP: ' | awk '{print $3}')
        echo "$ip - $country"
        # country=$(nordvpn status | grep Country)
        # ip=$(nordvpn status | grep Server)
        # echo -n "<fc=#4687ff><fn=1>嬨 </fn> $country - $ip </fc>  <fc=#666666>|</fc>"
    ;;
    *"Status: Disconnected"*)
        ip=$(ifconfig | grep inet | head -1 | awk '{print $2}')
        echo "$ip"
    ;;
esac