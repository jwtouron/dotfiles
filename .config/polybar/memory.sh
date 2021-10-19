#!/bin/sh

mem=$(free -t | awk '/Total/ {printf "%d", $3 / ($3 + $4) * 100.0}')
#mem=$(free | awk '/Mem/ { printf "%d", $3 / $2 * 100 }')

# if [ "$mem" -ge 90 ]; then
#     mem="%{F#f55}$mem%%{F-}"
# elif [ "$mem" -ge 70 ]; then
#     mem="%{F#ff0}$mem%%{F-}"
# else
#     mem="${mem}%"
# fi

# echo " $mem"

if [ "$mem" -ge 90 ]; then
    mem="%{o#f55} $mem%%{o-}"
elif [ "$mem" -ge 70 ]; then
    mem="%{o#ff0} $mem%%{o-}"
else
    mem=" ${mem}%"
fi

echo "$mem"
