#!/bin/sh

_lock() {
    for lock in i3lock-fancy i3lock xlock; do
        if command -v "$lock" >/dev/null; then
            "$lock"
            break
        fi
    done
}

options="1. Loc(k)\n2. Sus(p)end\n3. Lo(g)out\n4. (R)eboot\n5. Shutdo(w)n\n"
choice=$(printf "$options" | dmenu -i -n -c -bw 5 -l 5 | awk '{print $NF}')

case "$choice" in
    "Loc(k)") _lock ;;
    "Sus(p)end") _lock; systemctl suspend ;;
    "Lo(g)out") i3-msg exit ;;
    "(R)eboot") systemctl reboot ;;
    "Shutdo(w)n") systemctl poweroff ;;
esac
