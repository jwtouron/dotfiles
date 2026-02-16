#!/bin/sh

volume="$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{print $5}' | tr -d '%')"

notify-send \
    -h string:x-dunst-stack-tag:volume \
    -h "int:value:${volume}" "Volume"
