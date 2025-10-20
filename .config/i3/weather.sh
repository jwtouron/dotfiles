#!/bin/sh

set -e

if ! command -v wego >/dev/null; then
    echo "Error: Missing wego"
    exit 1
fi

if ! command -v jq >/dev/null; then
    echo "Error: Missing jq"
    exit 1
fi

MODE="$1" # "simple" or "detailed"
ST_BIN="$HOME/.config/st/st"
LOC="$(curl -sf ipinfo.io | jq -r '.loc')"
WEGO_ARGS="-units imperial -location $LOC"  # Fahrenheit, you can add more defaults here

# Compile st if missing
if [ ! -x "$ST_BIN" ]; then
    (cd ~/.config/st && make clean && make)
fi

if [ "$MODE" = "simple" ]; then
    current="$(wego $WEGO_ARGS -frontend json | jq .Current)"

    printf '%s' "$current" |
        jq -r '.Desc as $Desc |
            (.TempC * 1.8 + 32 | round) as $Temp |
            (.FeelsLikeC * 1.8 + 32 | round) as $FeelsLike |
            "🌤️ \($Desc) \($Temp) (\($FeelsLike)) °F"'
elif [ "$MODE" = "detailed" ]; then
    ~/.config/st/st -c WeatherPopup -g 130x40 -e sh -c "wego $WEGO_ARGS; read q"
else
    echo "Usage: $0 [simple|detailed]" >&2
    exit 1
fi

