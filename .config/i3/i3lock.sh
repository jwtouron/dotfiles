#!/bin/sh

command -v i3lock-fancy >/dev/null \
    || command -v i3lock >/dev/null \
    || notify-send "Missing Command" "i3lock-fancy/i3lock"

if command -v i3lock-fancy >/dev/null; then
    if command -v scrot >/dev/null; then
        i3lock-fancy -g -- scrot
    else
        i3lock-fancy -g
    fi
elif command -v i3lock >/dev/null; then
    i3lock -f -c 333333 --nofork
fi

