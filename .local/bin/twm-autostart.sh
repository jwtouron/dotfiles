#!/bin/sh

start() {
    FILE="$(basename $1)"
    pgrep -f "${FILE%%.*}" >/dev/null || "$1"
}

start cbatticon &
start nm-applet &
start picom &
start udiskie -n -s -a &
start ~/.local/bin/twm-update-icon.sh &
start ~/.local/bin/twm-daily-wallpaper.sh &

start parcellite || start xfce4-clipman &
