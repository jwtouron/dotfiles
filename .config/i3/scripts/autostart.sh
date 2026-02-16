#!/bin/sh
set -x

command="$1"
shift

# Kill existing matching processes, but not this script
pgrep -af -- "$command" \
    | awk -v me="$$" '$1 != me {print $1}' \
    | xargs -r kill

# Start the new command if it exists
if command -v "$command" >/dev/null || [ -f "$command" ]; then
    "$command" "$@"
else
    notify-send -u critical "Missing Command" "$command"
fi
