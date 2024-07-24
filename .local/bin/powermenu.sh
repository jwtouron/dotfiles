#!/bin/sh

pidof -o %PPID -x "powermenu.sh" >/dev/null && exit 1

# NOTE: In xfce4-power-manager-settings, under the "Security" tab,
# you may need to uncheck "Lock screen when system is going to sleep".
__lock() {
    if command -v i3lock-fancy >/dev/null; then
        i3lock-fancy -g &
    elif command -v i3lock >/dev/null; then
        i3lock -f -c 333333 &
    elif command -v xlock >/dev/null; then
        xlock &
    fi
}

__logout() {
    if pgrep i3 >/dev/null; then
        i3-msg exit
    elif pgrep herbstluftwm >/dev/null; then
        herbstclient quit
    fi
}

options="1. Loc(k)\n2. Sus(p)end\n3. Lo(g)out\n4. (R)eboot\n5. Shutdo(w)n\n"
choice=$(printf "$options" | fzfmenu --reverse | awk '{print $2}')

case "$choice" in
    "Loc(k)")     __lock ;;
    "Sus(p)end")  if ! pgrep xss-lock >/dev/null; then __lock; fi; systemctl suspend ;;  # Let xss-lock handle screen locking.
    "Lo(g)out")   loginctl kill-user '' ;;
    "(R)eboot")   systemctl reboot ;;
    "Shutdo(w)n") systemctl poweroff ;;
esac
