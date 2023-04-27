#!/bin/sh

__lock() {
    for lock in i3lock-fancy i3lock xlock; do
        if command -v "$lock" >/dev/null; then
            "$lock"
            break
        fi
    done
}

__logout() {
    if pgrep i3 >/dev/null; then
        i3-msg exit
    fi
}

options="1. Loc(k)\n2. Sus(p)end\n3. Lo(g)out\n4. (R)eboot\n5. Shutdo(w)n\n"
choice=$(printf "$options" | dmenu -i -n -c -bw 5 -l 5 | awk '{print $2}')

case "$choice" in
    "Loc(k)")     __lock ;;
    "Sus(p)end")  __lock; systemctl suspend ;;
    "Lo(g)out")   __logout ;;
    "(R)eboot")   systemctl reboot ;;
    "Shutdo(w)n") systemctl poweroff ;;
esac
