#!/bin/sh
# Output a simple, one-line description of the current
# weather using the National Weather Service

set -eu

if ! command -v jq >/dev/null; then
    echo "ERROR: jq is required" 1>&2 && exit 1
fi

LOC="$(curl -s ipinfo.io | jq -r '.loc')"
STATIONS_URL="$(curl -s https://api.weather.gov/points/$LOC | jq -r '.properties.observationStations')"
STATION_URL="$(curl -s $STATIONS_URL | jq -r '.observationStations[0]')"
WEATHER="$(curl -s $STATION_URL/observations/latest | jq -r '.properties')"
DESC="$(echo $WEATHER | jq -r '.textDescription')"
TEMP="$(echo $WEATHER | jq -r '.temperature')"
HEAT_INDEX="$(echo $WEATHER | jq -r '.heatIndex')"

if [ "$(echo $TEMP | jq -r '.unitCode')" = "wmoUnit:degC" ]; then
    TEMP=$(echo "$(echo $TEMP | jq -r .value) * 1.8 + 32" | bc)
else
    TEMP="$(echo $TEMP | jq -r '.value')"
fi

if [ "$(echo $HEAT_INDEX | jq -r '.unitCode')" = "wmoUnit:degC" ]; then
    HEAT_INDEX=$(echo "$(echo $HEAT_INDEX | jq -r .value) * 1.8 + 32" | bc)
else
    HEAT_INDEX="$(echo $HEAT_INDEX | jq -r '.value')"
fi

printf "%s %.1f(%.1f)°F\n" "$DESC" "$TEMP" "$HEAT_INDEX"
