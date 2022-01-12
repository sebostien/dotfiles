#!/bin/bash

# Used in xmobar

connected=$(nordvpn status | grep Status)

case "$connected" in
    *": Connected"*)
        country=$(nordvpn status | grep Country)
        ip=$(nordvpn status | grep Server)
        echo -n "<fc=#4687ff><fn=1>嬨 </fn> $country - $ip </fc>  <fc=#666666>|</fc>"
    ;;
esac
