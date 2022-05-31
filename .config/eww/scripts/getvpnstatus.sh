#!/bin/bash
status=$(nordvpn status)

case $(echo "$status" | grep Status) in
    *"Status: Connected"*)
        country=$(echo "$status" | grep Country | awk '{print $2}')
        city=$(echo "$status" | grep City | awk '{print $2}')
        echo "$city - $country"
    ;;
    *"Status: Disconnected"*)
        ip=$(ifconfig | grep inet | head -1 | awk '{print $2}')
        echo "$ip"
    ;;
esac