#!/usr/bin/sh

feh_random_background() {
    feh --bg-max --randomize --recursive --no-fehbg ~/.local/share/wallpapers /usr/share/wallpapers /usr/share/backgrounds || feh_random_background
}
feh_random_background
