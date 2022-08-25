#!/bin/sh

# Output the number of days since pacman last did an update, or -1 if it's never been updated.
# Note: Doesn't check for a successful update, or even that pacman is installed on the system.

last_update_date="$(awk 'match($0, "\\[([0-9]+-[0-9]+-[0-9]+).*\\] \\[PACMAN\\] starting full system upgrade", m) {x = m[1]} END {print x}' /var/log/pacman.log)"

if [ -z "$last_update_date" ]; then
    echo "-1"
    exit
fi

last_update_sec="$(date +%s -d $last_update_date)"
now_sec="$(date +%s)"

echo "$((($now_sec - $last_update_sec) / 86400))"
