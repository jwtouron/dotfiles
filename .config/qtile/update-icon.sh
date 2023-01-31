#!/bin/sh

if command -v pacman >/dev/null; then
    pkgmgr=pacman
elif command -v apt >/dev/null; then
    pkgmgr=apt
elif command -v dnf >/dev/null; then
    pkgmgr=dnf
elif command -v xbps-install >/dev/null; then
    pkgmgr=xbps
fi

list() {
    case "$pkgmgr" in
        pacman) checkupdates ;;
        apt)    apt list --upgradable 2>/dev/null | grep upgradable ;;
        dnf)    dnf updateinfo -q --list ;;
        xbps)   xbps-install -nuM | wc -l ;;
    esac
}

limit() {
    case "$pkgmgr" in
        pacman)
            last_update_days="$(pacman-last-update.sh)"
            if [[ "$last_update_days" -ge 7 ]]; then
                echo 1
            else
                echo "$(($(pacman -Q | wc -l) * 100 / 2000))"
            fi
            ;;
        *) echo 1 ;;
    esac
}

[[ -z "$pkgmgr" ]] && exit 1

cache_loc="$HOME/.cache/updates"
yad_pid=

while true; do
    list > "$cache_loc"
    count="$(wc -l < $cache_loc)"
    if [[ "$count" -ge "$(limit)" ]]; then
        if [[ -n "$yad_pid" ]]; then
            kill "$yad_pid"
        fi
        yad --notification --text="$count Updates Available" --image="$HOME/.config/qtile/lotsupdate.png" &
        yad_pid=$!
    fi
    sleep $((86400 / 4))
done
