#!/bin/sh

killall -q -w polybar

for m in $(polybar --list-monitors | cut -d":" -f1); do
    MONITOR=$m polybar --reload spectrwm &
done
