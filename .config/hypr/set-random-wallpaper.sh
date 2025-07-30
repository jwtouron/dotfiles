#!/usr/bin/bash

set -e

~/.local/bin/command-notify.sh magick

files="$(find -L ~/.local/share/wallpapers /usr/share/backgrounds -type f \( -name \*.png -o -name \*.jpg \) | sort --random-sort)"
for file in $files; do
    darkness="$(magick $file -colorspace Gray -format %\[fx:mean\] info: | awk '{ print 1 - $1 }')"
    if [ "$(echo $darkness | awk '{ print ($1 > 0.5) }')" -eq 1 ]; then
        hyprctl hyprpaper unload all >/dev/null
        hyprctl hyprpaper preload "$file" >/dev/null
        hyprctl hyprpaper wallpaper ",$file" >/dev/null
        break
    fi
done
