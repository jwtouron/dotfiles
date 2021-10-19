#!/bin/sh

case $BLOCK_BUTTON in
    1) yad --calendar --undecorated --mouse --no-buttons --close-on-unfocus ;;
    3) xterm -class xterm-popup -e calcurse ;;
esac

date +"%a, %d %b %Y, %H:%M"
