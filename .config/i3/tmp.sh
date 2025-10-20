#!/bin/sh

set -x

ipinfo="$(curl -sf ipinfo.io)"

url='https://api.open-meteo.com/v1/forecast'
url="$url?latitude=$(printf '%s' "$ipinfo" | jq -r '.loc' | sed 's/,/\&longitude=/')"
url="$url&timezone=$(printf '%s' "$ipinfo" | jq -r '.timezone')"
url="$url&current=temperature_2m,relative_humidity_2m,apparent_temperature,weather_code"
echo "$url"
json="$(curl -f "$url")"

echo "$json" | jq -r

# https://api.open-meteo.com/v1/forecast?latitude=52.52&longitude=13.41&hourly=temperature_2m,weather_code&current=temperature_2m,relative_humidity_2m,apparent_temperature,weather_code&forecast_days=3&wind_speed_unit=mph&temperature_unit=fahrenheit&precipitation_unit=inch

exit 0

json="$(wego -l "$(curl -sf ipinfo.io | jq -r '.loc')" -f json)"

header="$(printf '%s' "$json" | jq -r '.Current | "\(.Desc) \(.TempC * 1.8 + 32 | round)°\nFeels like: \(.FeelsLikeC | round)°\nHumidity: \(.Humidity)%\n"')"

printf '%s\n\n' "$header"

printf '%s' "$json" | jq -r '.Forecast | map("\(.)") | .[]' | while read -r day; do
    date="$(date -d "$(printf '%s' "$day" | jq -r '.Date')" +'%Y-%m-%d')"
    if [ "$date" = "$(date -d 'today' +'%Y-%m-%d')" ]; then
        printf '%s' 'Today, '
    elif [ "$date" = "$(date -d 'tomorrow' +'%Y-%m-%d')" ]; then
        printf '%s' 'Tomorrow, '
    fi
    echo "$date"

    high="$(printf '%s' "$day" | jq '.Slots | map(.TempC * 1.8 + 32 | round) | max')"
    low="$(printf '%s' "$day" | jq '.Slots | map(.TempC * 1.8 + 32 | round) | min')"


    sunrise="$(printf '%s' "$day" | jq -r '.Astronomy.Sunrise' | xargs -I{} date -d {} +'%H:%M')"
    sunset="$(printf '%s' "$day" | jq -r '.Astronomy.Sunset' | xargs -I{} date -d {} +'%H:%M')"

    printf '⬆️ %s° ⬇️ %s° 🌅 %s 🌇 %s\n' "$high" "$low" "$sunrise" "$sunset"

    printf '%s' "$day" | jq -r '.Slots.[] | "\(.)"' | while read -r slot; do
        hour="$(printf '%s' "$slot" | jq -r '.Time' | xargs -I{} date -d '{}' +'%H')"
        code="$(printf '%s' "$slot" | jq -r '.Code')"
        # code="$(printf '%s' "$slot" | jq -r '.Code' | xargs -I{} awk -v code={} '$1 ~ code {print $2}' "$(dirname "$(realpath "$0")")/code-icons.txt")"
        temp="$(printf '%s' "$slot" | jq -r '.TempC * 1.8 + 32 | round')"
        desc="$(printf '%s' "$slot" | jq -r '.Desc')"
        printf '%s %s %s° %s\n' "$hour" "$code" "$temp" "$desc"
    done

    # for slot in $(printf '%s' "$day" | jq -r '.Slots.[] | "\(.)"'); do
    #     echo "$slot"
    #     echo
    #     # printf '%s\n' "$slot" | jq -r '.Time' | xargs -I{} date -d '{}' +'%H'
    # done


    # echo "$day" | jq -r '.Slots'

    # printf '%s' "$day" | jq -r '.Slots | map("\(.Time)|\(.TempC * 1.8 + 32 | round)°|\(.Desc)") | .[]' | awk -F '|' '{system("printf '\''%s %s %s\n'\'' \"$(date -d " $1 " +%H)\" \"" $2 "\" \"" $3 "\"");}'
    echo
done
