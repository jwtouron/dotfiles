#!/bin/sh

set -eu

background=$(find /usr/share/backgrounds /usr/share/wallpapers ~/.local/share/wallpapers -type f \( -name '*.jpg' -o -name '*.png' \) 2>/dev/null | shuf -n1)

mkdir -p ~/.config/nitrogen
echo -n "" > ~/.config/nitrogen/bg-saved.cfg

for i in `seq 0 $(( $(xrandr | grep -c '\bconnected\b') - 1))`; do
    cat >> ~/.config/nitrogen/bg-saved.cfg <<EOF
[xin_$i]
file=$background
mode=5
bgcolor=#000000

EOF
done

nitrogen --restore
