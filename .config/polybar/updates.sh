#!/bin/sh

cache_loc="$HOME/.cache/updates"

if command -v pacman >/dev/null; then
    update_cmd="sudo pacman -Syu"
    list() { checkupdates ;}
elif command -v apt >/dev/null; then
    update_cmd="sudo apt update && sudo apt upgrade"
    list() { apt list --upgradable 2>/dev/null | grep upgradable ;}
elif command -v dnf >/dev/null; then
    update_cmd="sudo dnf upgrade"
    list() { dnf updateinfo -q --list ;}
fi

case "$1" in
    "click-left")
        xterm -class xterm-popup -geometry 125x35 -e "cat $cache_loc; read" ;;
    "click-right")
        xterm -class xterm-popup -geometry 125x35 -e "$update_cmd; read" ;;
esac

list > "$cache_loc"

count="$(wc -l < $cache_loc)"
if command -v pacman >/dev/null && [ "$count" -lt 50 ]; then
    count='-'
fi

echo " $count "
