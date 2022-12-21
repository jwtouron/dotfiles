#!/bin/sh

read cpu a b c previdle rest < /proc/stat
prevtotal=$((a+b+c+previdle))
sleep 0.5
read cpu a b c idle rest < /proc/stat
total=$((a+b+c+idle))
cpu=$((100*( (total-prevtotal) - (idle-previdle) ) / (total-prevtotal) ))

if [ "$cpu" -ge 90 ]; then
    foreground='red'
elif [ "$cpu" -ge 70 ]; then
    foreground='yellow'
else
    foreground='#556677'
fi

printf "<span weight='bold'>   <span foreground='$foreground'>%3d%%</span></span>" "$cpu"
