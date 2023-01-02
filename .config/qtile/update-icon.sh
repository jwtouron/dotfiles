#!/bin/sh

cache_loc="$HOME/.cache/updates"

if command -v pacman >/dev/null; then
    list() { checkupdates ;}
    limit=$(($(pacman -Q | wc -l) * 100 / 2000))
elif command -v apt >/dev/null; then
    list() { apt list --upgradable 2>/dev/null | grep upgradable ;}
    limit=1
elif command -v dnf >/dev/null; then
    list() { dnf updateinfo -q --list ;}
    limit=1
elif command -v xbps-install >/dev/null; then
    list() { xbps-install -nuM | wc -l ;}
    limit=1
fi

# last_update_days="$(pacman-last-update.sh)"

yad_pid=

while true; do
    list > "$cache_loc"
    count="$(wc -l < $cache_loc)"
    if [[ "$count" -ge "$limit" ]]; then
        image="$HOME/.config/qtile/lotsupdate.png"
    # else
    #     image="$HOME/.config/qtile/noupdate.png"
    fi
    if [[ -n "$yad_pid" ]]; then
        kill "$yad_pid"
    fi
    yad --notification --text="$count Updates Available" --image="$image" --command='' &
    yad_pid=$!
    sleep $((86400 / 4))
done
