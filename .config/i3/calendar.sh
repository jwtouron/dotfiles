#!/bin/sh

if [ "$TERMINAL" = "kitty" ]; then
    i3-sensible-terminal sh -c 'pal --nocolor -r 28 | less'
else
    i3-sensible-terminal -e "sh -c 'pal --nocolor -r 28 | less'"
fi
