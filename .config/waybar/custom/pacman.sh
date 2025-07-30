#!/bin/sh

updates=$(checkupdates --nocolor)
update_count=$(echo "$updates" | wc -l)

total_installed=$([ "$update_count" -gt 0 ] && (pacman -Q | wc -l) || echo -n 0)

tooltip=$(echo -n "$updates" | awk '!/^$/ { printf "%s\\\\n", $0 }' | tr -d '\n')
# tooltip=$(echo -n "$updates" | awk '{printf "%s\\\\n", $0} END {print ""}')

# echo "$tooltip"

echo { \"text\": \""$update_count"\", \"tooltip\": \""$tooltip"\", \"class\": \"pacman\" } #| jq --unbuffered --compact-output
