#!/bin/sh

ds=@DEFAULT_SINK@

case $BLOCK_BUTTON in
    1) pavucontrol ;;
    3) pactl set-sink-mute "$ds" toggle ;;
    4) pactl set-sink-volume "$ds" +5% ;;
    5) pactl set-sink-volume "$ds" -5% ;;
esac

if [ "$(pactl get-sink-mute $ds)" = "Mute: yes" ]; then
    echo " muted"
else
    vol=$(pactl get-sink-volume $ds | tr -d '\n' | sed -E 's|.*[^[:digit:]]([[:digit:]]+)%.*[^[:digit:]]([[:digit:]]+)% .*|\1 \2|' | awk '{ print ($1 + $2) / 2}')
    if [ "$vol" -ge 50 ]; then
        echo " $vol%"
    elif [ "$vol" -eq 0 ]; then
        echo " $vol%"
    else
        echo " $vol%"
    fi
fi
