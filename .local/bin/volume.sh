#!/usr/bin/env bash

help() {
    echo "volume.sh [up N|down N|mute]"
}

if [ -z "$1" ] || [ "$1" != "mute" ] && [ -z "$2" ]; then
    help
    exit 1
fi

set -euo pipefail

_notify_send() {
    notify-send -a myvolume -r 12345 Volume "$1"
}

_command() {
    command -v "$1" >/dev/null
}

if _command amixer; then
    _getvolume() {
        amixer -D pulse sget Master | awk -F '[][]' '/Front (Left|Right):/ { x = x + $2 } END { print x / 2 }'
    }
    _ismuted() {
        amixer -D pulse sget Master | grep -q 'Front Left: .*\[off\]'
    }
    up() {
        amixer -q -D pulse sset Master "${1}%+"
    }
    down() {
        amixer -q -D pulse sset Master "${1}%-"
    }
    mute() {
        amixer -q -D pulse sset Master toggle
    }
elif _command pactl; then
    _getvolume() {
        pactl get-sink-volume '@DEFAULT_SINK@' | awk -F '/' '/^Volume/ { print(($2 + $4) / 2) }'
    }
    _ismuted() {
        test "$(pactl get-sink-mute @DEFAULT_SINK@)" = "Mute: yes"
    }
    up() {
        pactl set-sink-volume '@DEFAULT_SINK@' "+${1}%"
    }
    down() {
        pactl set-sink-volume '@DEFAULT_SINK@' "-${1}%"
    }
    mute() {
        pactl set-sink-mute '@DEFAULT_SINK@' toggle
    }
else
    echo "No volume program available." >&2
    exit 1
fi

case "$1" in
    "up") up "$2" && _notify_send "$(_getvolume)" ;;
    "down") down "$2" && _notify_send "$(_getvolume)" ;;
    "mute") mute; _ismuted && _notify_send muted ;;
    *) help; exit 1 ;;
esac

# vim: set ft=bash
