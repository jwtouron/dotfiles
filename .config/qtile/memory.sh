#!/bin/sh

mem=$(free -t | awk '/Total/ {printf "%d", $3 / ($3 + $4) * 100.0}')


if [ "$mem" -ge 90 ]; then
    foreground='red'
elif [ "$mem" -ge 70 ]; then
    foreground='yellow'
else
    foreground='#556677'
fi

printf "<span weight='bold'>   <span foreground='$foreground'>%3d%%</span></span>" "$mem"
