#!/bin/sh

cd "$(dirname "$(realpath "$0")")"

killall polybar && sleep 0.1

WIDTH="$(xrandr | sed -E -n 's/^Screen [0-9]+:.*current +([0-9]+) .*/\1/p')"
WIDTH="$(( $WIDTH -40 ))"
WIDTH="$WIDTH" polybar -r -c ./polybar.ini
