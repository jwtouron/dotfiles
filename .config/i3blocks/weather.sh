#!/bin/sh

case $BLOCK_BUTTON in
    1) xterm -class xterm-popup -geometry 125x45 -e ~/.config/i3blocks/weather-click.sh
        ;;
esac

loc=$(cat ~/.cache/wttr 2>/dev/null | tr -d '[[:space:]]')

if [ -n "$loc" ]; then
    loc="/$loc"
fi

curl -Ss "https://wttr.in${loc}?0&T&Q" | cut -c 16- | head -2 | xargs echo "⛅"

# weather_icons="🌙🌕🌘🌒🌦️🌤️☀️🌩️❄️☁️🌧️"
