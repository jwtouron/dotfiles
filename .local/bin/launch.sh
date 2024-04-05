#!/bin/sh

file_manager() (
    if command pcmanfm >/dev/null; then
        pcmanfm
    fi
)

options="Browser
Emacs
File Manager
HTop
Volume"

# if command -v dmenu >/dev/null; then
    choice=$(echo "$options" | awk '{print NR ". " $0}' | fzfmenu --reverse --prompt "Launch: " | awk '{$1=""; print $0}')
# else
    # choice=$(echo "$options" | awk '{print NR ". " $0}' | rofi -dmenu -i -p 'Launch' -case-sensitive -auto-select -theme ~/.config/rofi/rounded-nord-dark.rasi | awk '{$1=""; print $0}')
# fi

case "$choice" in
    " Browser") $BROWSER ;;
    " Emacs") emacs ;;
    " File Manager") file_manager ;;
    " HTop") xterm -class xterm-floating -geometry 120x40 -e htop ;;
    " Volume") pavucontrol ;;
esac
