#!/bin/sh
# Output a simple, one-line description of the current
# weather using WeatherAPI

set -euo pipefail

if ! command -v jq >/dev/null; then
    echo "ERROR: jq is required" 1>&2 && exit 1
fi

APIKEY="$(awk '/ *[^ ]/ { print $1 }' $HOME/.local/share/weatherapi | head -n1)"

if [ -z "$APIKEY" ]; then
    echo "ERROR: API key not found at $HOME/.local/share/weatherapi" 1>&2 && exit 1
fi

ZIPCODE="${1:-$(curl -s ipinfo.io | jq -r '.postal')}"

forecast=$(curl -s "http://api.weatherapi.com/v1/forecast.json?key=${APIKEY}&q=${ZIPCODE}&days=3")

header() {
    echo $forecast | jq -r '[ .location.name, .location.region, .location.localtime, .current.last_updated, .current.condition.text, .current.temp_f, .current.feelslike_f, .current.precip_in, .current.wind_dir, .current.wind_mph ] | @sh' | xargs printf "%s, %s, $ZIPCODE\n\nCurrent:\n\nLocal Time:     %s\nLast Updated:   %s\nCurrent Temp:   %s %s(%s)°F\nPrecipitation:  %s in\nWind:           %s %s mph"
}

hourly() {
    echo "Hourly:"
    echo ""
    echo "Date             | Temp    | Rain | Snow | Description"
    echo "-----------------+---------+------+------+------------------------------"
    echo "$forecast" | jq -r '.forecast.forecastday[0].hour[] | [ .time, .temp_f, .chance_of_rain, .chance_of_snow, .condition.text ] | @sh' | xargs printf "%-16s | %5.1f°F | %3s%% | %3s%% | %s\n"
    echo ""
    echo "Date             | Temp    | Rain | Snow | Description"
    echo "-----------------+---------+------+------+------------------------------"
    echo "$forecast" | jq -r '.forecast.forecastday[1].hour[] | [ .time, .temp_f, .chance_of_rain, .chance_of_snow, .condition.text ] | @sh' | xargs printf "%-16s | %5.1f°F | %3s%% | %3s%% | %s\n"
} 

forecast() {
    echo "3 Day Forecast:"
    echo ""
    echo "$forecast" | jq -r '.forecast.forecastday[] |
                              [
                                  .date_epoch,
                                  .day.condition.text,
                                  .day.mintemp_f,
                                  .day.avgtemp_f,
                                  .day.maxtemp_f,
                                  .day.daily_chance_of_rain,
                                  .day.daily_chance_of_snow,
                                  .day.totalprecip_in,
                                  .day.totalsnow_cm,
                                  .day.avghumidity,
                                  .day.maxwind_mph
                              ] | join("<<<>>>")' | \
        # Date ($1)
        # description ($2)
        # mintemp ($3)   rain% ($6)    rainin ($8)
        # avgtemp ($4)   snow% ($7)    snowin ($9)
        # maxtemp ($5)   avghum ($10)  maxwind ($11)
        awk -F'<<<>>>' '{
                system("TZ=GMT date -d @"$1" +%A,\\ %d\\ %B\\ %Y")
                print "  Condition: " $2
                print sprintf("  %-10s%7s | %-14s%4s | %-9s%10s", "Min Temp:", sprintf("%5.1f°F",$3), "Rain Chance:", $6"%", "Rain Amt:", $8" in")
                print sprintf("  %-10s%7s | %-14s%4s | %-9s%10s", "Avg Temp:", sprintf("%5.1f°F",$4), "Snow Chance:", $7"%", "Snow Amt:", ($9)" in")
                print sprintf("  %-10s%7s | %-14s%4s | %-9s%10s", "Max Temp:", sprintf("%5.1f°F",$5), "Avg Humidity:", $10"%", "Max Wind:", $11" mph")
                print ""
             }'
}

header
echo ""
echo ""
forecast
hourly
