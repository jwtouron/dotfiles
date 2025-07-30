#!/bin/sh

command -v "$1" >/dev/null
exit_code=$?

if [ $exit_code -ne 0 ]; then
    notify-send -u critical "Missing Application" "The following application could not be found: $1"
fi

exit $exit_code
