#!/bin/sh

loc=$(cat ~/.cache/wttr 2>/dev/null | tr -d '[[:space:]]')

if [ -n "$loc" ]; then
    loc="/$loc"
fi

#curl -Ss "https://wttr.in${loc}?0&T&Q" | cut -c 16- | head -2 | xargs echo "⛅"
weather=$(curl -Ssk "https://wttr.in${loc}?0&T&Q" 2>/dev/null | cut -c 16- | head -2 | xargs echo " ")
echo " $weather "
