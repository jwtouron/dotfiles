#!/bin/sh

read cpu a b c previdle rest < /proc/stat
prevtotal=$((a+b+c+previdle))
sleep 0.5
read cpu a b c idle rest < /proc/stat
total=$((a+b+c+idle))
cpu=$((100*( (total-prevtotal) - (idle-previdle) ) / (total-prevtotal) ))

# if [ "$cpu" -ge 90 ]; then
#     cpu="%{F#f55}$cpu%%{F-}"
# elif [ "$cpu" -ge 70 ]; then
#     cpu="%{F#ff0}$cpu%%{F-}"
# else
#     cpu="${cpu}%"
# fi

# echo "  $cpu"

if [ "$cpu" -ge 90 ]; then
    cpu="%{o#f55} $cpu%%{-o}"
elif [ "$cpu" -ge 70 ]; then
    cpu="%{o#ff0} $cpu%%{-o}"
else
    cpu=" ${cpu}%"
fi

echo " $cpu "
