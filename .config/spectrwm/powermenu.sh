#!/bin/sh

options="1. Loc(k)\n2. Sus(p)end\n3. Lo(g)out\n4. (R)eboot\n5. Shutdo(w)n\n"
choice=$(printf "$options" | fzfmenu --no-sort --reverse --exact --prompt='Goodbye! ' | awk '{print $NF}')
# choice=$(printf "$options" | dmenu -i -n -c -bw 5 -l 5 | awk '{print $NF}')

case "$choice" in
    "Loc(k)") $HOME/.local/bin/lock ;;
    "Sus(p)end") $HOME/.local/bin/lock ; systemctl suspend ;;
    "Lo(g)out") pkill spectrwm ;;
    "(R)eboot") systemctl reboot ;;
    "Shutdo(w)n") systemctl poweroff ;;
esac
