#!/bin/sh
# Outputs the latitude and longitude of the provided zipcode on separate lines.

if ! command -v jq >/dev/null; then
    printf "ERROR: jq is required" 1>&2 && exit 1
fi

# App ID borrowed from Qtile
APPID=${OPENWEATHER_API_KEY:-7834197c2338888258f8cb94ae14ef49}
ZIPCODE="${1:-$(curl -s ipinfo.io | jq -r '.postal')}"

curl -s "http://api.openweathermap.org/geo/1.0/zip?zip=$ZIPCODE,US&appid=$APPID" | jq '.lat, .lon'
