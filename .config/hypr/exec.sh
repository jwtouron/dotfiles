#!/bin/sh

# https://specifications.freedesktop.org/icon-naming-spec/latest/#names

if ! pgrep "$1"; then
    if command -v "$1" >/dev/null; then
        "$@" &
        exit 0
    else
        notify-send --icon emblem-important --urgency critical "Missing Application" \
            "The following application could not be found: $1"
        exit 1
    fi
fi
