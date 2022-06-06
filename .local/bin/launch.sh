#!/bin/sh

browser() (
    for browser in \
        "firefox" \
        "brave" \
        "brave-browser" \
        "chromium" \
        "chromium-browser" \
        "google-chrome"
    do
        if type "$browser" >/dev/null 2>&1; then
            "$browser";
            break
        fi
    done
)

file_manager() (
    if type pcmanfm >/dev/null; then
        pcmanfm
    fi
)

options="Browser
Emacs
File Manager
HTop
Volume"

choice=$(echo "$options" | awk '{print NR ". " $0}' | dmenu -f -i -n -l 6 -c -bw 5 -p 'Launch:' | awk '{$1=""; print $0}')

case "$choice" in
    " Browser") browser ;;
    " Emacs") emacs ;;
    " File Manager") file_manager ;;
    " HTop") xterm -class xterm-popup -e htop ;;
    " Volume") pavucontrol ;;
esac
