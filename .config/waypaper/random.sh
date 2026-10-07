#!/bin/sh

if ! command -v waypaper >/dev/null; then
    notify-send "Missing Command" waypaper
    exit 1
fi

case "$(printf '%s' "${XDG_CURRENT_DESKTOP:-}" | tr '[:upper:]' '[:lower:]')" in
    *hyprland*)
        backend=hyprpaper
        ;;
    *sway*)
        backend=swaybg
        ;;
    *)
        notify-send "Unknown Desktop" \
            "XDG_CURRENT_DESKTOP=${XDG_CURRENT_DESKTOP:-unset}"
        exit 1
        ;;
esac

if ! command -v "$backend" >/dev/null; then
    notify-send "Missing Wallpaper Backend" "$backend"
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
        if [ "$(printf '%f' "$darkness" | awk '{ print ($1 > 0.667) }')" -eq 1 ]; then
            break
        fi
    done
fi

if ! waypaper --backend "$backend" --wallpaper "$file"; then
    notify-send "Wallpaper Error" \
        "Failed to set wallpaper using $backend"
    exit 1
fi
