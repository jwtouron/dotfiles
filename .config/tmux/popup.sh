#!/bin/sh

if [ "$(tmux display-message -p -F "#{session_name}")" = "popup" ]; then
    tmux detach-client || true
else
    tmux popup -E -h "90%" -w "90%" -b rounded -S 'fg=#5eacd3' "tmux attach -t popup || tmux new -s popup" || true
fi
