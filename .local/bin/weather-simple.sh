#!/bin/sh
# Output a simple, one-line description of the current
# weather using WeatherAPI

set -eu

if ! command -v jq >/dev/null; then
    echo "ERROR: jq is required" 1>&2 && exit 1
fi

[ ! -f "$HOME/.local/share/weatherapi" ] && echo "Missing WeatherAPI Key" && exit 0

APIKEY="$(awk '/ *[^ ]/ { print $1 }' $HOME/.local/share/weatherapi | head -n1)"

if [ -z "$APIKEY" ]; then
    echo "ERROR: API key not found at $HOME/.local/share/weatherapi" 1>&2 && exit 1
fi

ZIPCODE="${1:-$(curl -s ipinfo.io | jq -r '.postal')}"

curl -s "http://api.weatherapi.com/v1/current.json?key=${APIKEY}&q=${ZIPCODE}" \
     | jq -r '.current.condition.text + " " + (.current.temp_f|tostring) + "(" + (.current.feelslike_f|tostring+")°F")'
