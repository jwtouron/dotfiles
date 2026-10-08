#!/bin/sh

set -x

valid_geometry() {
    printf '%s\n' "$1" |
        grep -Eq '^-?[0-9]+,-?[0-9]+ [1-9][0-9]*x[1-9][0-9]*$'
}

ensure_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        notify-send 'Missing Command' "$1"
        exit 1
    fi
}

ensure_command fuzzel
ensure_command grim
ensure_command jq
ensure_command slurp
ensure_command wl-copy

screenshot_dir="$HOME/Pictures/Screenshots"
mkdir -p "$screenshot_dir"

screenshot_path="$screenshot_dir/Screenshot-$(date +'%Y-%m-%d_%H-%M-%S').png"

scope="$(
    printf 'Screen\nWindow\nRegion\n' \
        | fuzzel --dmenu --mesg='Screenshot Scope' --lines=3 --selection-radius=20 \
            --namespace=screenshot-picker
)"

case "$scope" in
    Screen)
        grim "$screenshot_path"
        ;;
    Window)
        geometry="$(
            hyprctl activewindow -j |
                jq -r '"\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"'
        )"
        valid_geometry "$geometry" || exit 0
        grim -g "$geometry" "$screenshot_path"
        ;;
    Region)
        geometry="$(slurp -d)" || exit 0
        valid_geometry "$geometry" || exit 0
        grim -g "$geometry" "$screenshot_path"
        ;;
    *)
        exit 0
        ;;
esac

if [ "$?" -eq 0 ]; then
    copy_to_clipboard="$(
        printf 'No\nYes\n' \
            | fuzzel --dmenu --mesg='Copy to Clipboard?' --lines=2 \
                --selection-radius=20 --width 18
    )"
    if [ "$copy_to_clipboard" = "Yes" ]; then
        setsid -f wl-copy --type image/png < "$screenshot_path"
    fi

    action="$(
        notify-send \
            --action=open='Open Screenshot' \
            --expire-time=5000 \
            'Screenshot Saved' \
            "$screenshot_path"
    )"
    [ "$action" = open ] && xdg-open "$screenshot_path"
fi
