#!/bin/sh

start() {
    FILE="$(basename $1)"
    pgrep "${FILE%%.*}" >/dev/null || "$1" &
}

start cbatticon
start nm-applet
start picom
start ~/.local/bin/twm-update-icon.sh

for cmd in parcellite xfce4-clipman; do
    if command -v "$cmd" >/dev/null; then
        if ! pgrep "$cmd" >/dev/null; then
            "$cmd" &
            break
        fi
    fi
done
