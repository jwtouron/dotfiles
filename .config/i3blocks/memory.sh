#!/bin/sh

mem=$(free -t | awk '/Total/ {printf "%d", $3 / ($3 + $4) * 100.0}')
#mem=$(free | awk '/Mem/ { printf "%d", $3 / $2 * 100 }')

if [ "$mem" -ge 90 ]; then
    mem="<span foreground='#ff5555'>$mem</span>"
elif [ "$mem" -ge 70 ]; then
    mem="<span foreground='#ffff00'>$mem</span>"
fi

echo " ${mem}%"
