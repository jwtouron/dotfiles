#!/bin/sh

mem=$(free -t | awk '/Total/ {printf "%d", $3 / ($3 + $4) * 100.0}')


if [ "$mem" -ge 90 ]; then
    foreground='red'
elif [ "$mem" -ge 70 ]; then
    foreground='yellow'
else
    foreground='black'
fi

mem="<span foreground='$foreground'>$mem</span>"
mem="<span weight='bold'>$mem%</span>"

echo " $mem"
