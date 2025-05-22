#!/bin/sh

lines="$(for arg in "$@"; do
    echo "$arg"
done)"

echo "$lines" | kitten clipboard

if command -v notify-send >/dev/null; then
    notify-send "Copied!" "$lines"
elif command -v terminal-notifier >/dev/null; then
    echo "Copied!\n$lines" | terminal-notifier
fi
