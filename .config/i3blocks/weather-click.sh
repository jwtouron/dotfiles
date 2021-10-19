#!/bin/sh

loc=$(cat ~/.cache/wttr 2>/dev/null | tr -d '[[:space:]]')

if [ -z "$loc" ]; then
    echo -n 'Location: '; read loc
    echo "$loc" > ~/.cache/wttr
    clear
fi

curl -Ss "https://wttr.in/$loc"
read x
