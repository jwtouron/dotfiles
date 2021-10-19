#!/bin/sh

ds=@DEFAULT_SINK@

case "$1" in
    "inc") pactl set-sink-volume "$ds" "+${2}%" ;;
    "dec") pactl set-sink-volume "$ds" "-${2}%" ;;
    "mute") pactl set-sink-mute "$ds" toggle ;;
esac

if [ "$(pactl get-sink-mute $ds)" = "Mute: yes" ]; then
    notify-send -h string:x-canonical-private-synchronous:myvolume Volume muted
else
    notify-send -h string:x-canonical-private-synchronous:myvolume Volume "$(pactl get-sink-volume $ds | tr -d '\n' | sed -E 's|.*[^[:digit:]]([[:digit:]]+)%.*[^[:digit:]]([[:digit:]]+)% .*|\1 \2|' | awk '{ print ($1 + $2) / 2}')"
fi
