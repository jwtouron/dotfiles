#!/bin/sh

has_magick=
command -v magick >/dev/null && has_magick=1
[ -z "$has_magick" ] && notify-send "Missing Command" magick

while true; do
    files="$(find -L ~/.local/share/wallpapers /usr/share/backgrounds /usr/share/wallpapers -type f \( -name \*.png -o -name \*.jpg \) | sort --random-sort)"
    file="$(printf %s "$files" | head -n1)"
    if [ -n "$has_magick" ]; then
        for file in $files; do
            darkness="$(magick "$file" -crop x30+0+0 -colorspace Gray -format %\[fx:mean\] info: | awk '{ print 1 - $1 }')"
            if [ "$(echo $darkness | awk '{ print ($1 > 0.5) }')" -eq 1 ]; then
                break
            fi
        done
    fi
    xwallpaper --zoom "$file"
    sleep 1d
done
