#!/bin/sh
# Output a simple, one-line description of the current weather

set -eu

if ! command -v jq >/dev/null; then
    echo "ERROR: jq is required" 1>&2 && exit 1
fi

if ! command -v wego >/dev/null; then
    echo "ERROR: wego is required" 1>&2 && exit 1
fi

LOC="$(curl -s ipinfo.io | jq -r '.loc')"
WEATHER="$(wego -frontend=json $LOC | jq -r .Current)"

echo "$WEATHER" |
    jq -r '.Desc as $Desc |
           (.TempC * 1.8 + 32 | round) as $Temp |
           (.FeelsLikeC * 1.8 + 32 | round) as $FeelsLike |
           "🌤️ \($Desc) \($Temp) (\($FeelsLike)) °F"'
