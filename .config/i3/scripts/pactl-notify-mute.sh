#!/bin/sh

mute="$(pactl get-sink-mute @DEFAULT_SINK@)"

notify-send \
    -h string:x-dunst-stack-tag:volume \
    "Volume" "$mute"
