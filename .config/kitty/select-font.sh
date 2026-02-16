#!/bin/sh

set -eu

cd "$(dirname "$(realpath "$0")")"

font="$(kitty list-fonts 2>/dev/null)"

if printf '%s' "$font" | grep -q font_family; then
    read -p "Font size [14]: " size
    [ -z "$size" ] && size=14
    printf '%s\nfont_size        %s\n' "$font" "$size" > current-font.conf

fi
