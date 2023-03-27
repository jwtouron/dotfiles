#!/bin/sh

start() {
    local FILE
    FILE="$(basename $1)"
    pgrep "${FILE%%.*}" >/dev/null || "$1" &
}

start cbatticon
start nm-applet
start picom
start ~/.local/bin/twm-update-icon.sh
