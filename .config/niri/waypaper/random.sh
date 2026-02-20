#!/bin/sh

if ! command -v waypaper >/dev/null; then
    notify-send "Missing Command" waypaper
    exit 1
fi

has_magick=
command -v magick >/dev/null && has_magick=1
[ -z "$has_magick" ] && notify-send "Missing Command" magick

files="$(find -L ~/.local/share/wallpapers /usr/share/backgrounds /usr/share/wallpapers -type f \( -name \*.png -o -name \*.jpg \) | sort --random-sort)"
file="$(printf %s "$files" | head -n1)"

if [ -n "$has_magick" ]; then
    for file in $files; do
        darkness="$(magick "$file" -colorspace Gray -format %\[fx:mean\] info: | awk '{ print 1 - $1 }')"
        if [ "$(echo $darkness | awk '{ print ($1 > 0.5) }')" -eq 1 ]; then
            break
        fi
    done
fi

waypaper --wallpaper "$file"
