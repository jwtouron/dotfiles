#!/bin/sh

workspaces() {
    # How many numbered workspaces you want to show
    N=6

    # Cache i3 state to avoid calling i3-msg repeatedly
    tree_json="$(i3-msg -t get_tree)"

    # Current focused workspace number
    focused="$(i3-msg -t get_workspaces | jq -r '.[] | select(.focused==true).num')"

    # Compute wrap-around next/prev based on focused
    # next = focused % N + 1 ; prev = ((focused + N - 2) % N) + 1
    next_ws=$(( focused % N + 1 ))
    prev_ws=$(( (focused + N - 2) % N + 1 ))

    for n in $(seq 1 "$N"); do
        # Window count for this workspace (0 if it doesn't currently exist)
        count="$(printf '%s' "$tree_json" | jq --argjson n "$n" '
            (.. | objects | select(.type=="workspace" and .num==$n)) as $ws
            | [$ws | .. | objects | .window | select(. != null)] | length
        ')"
        count="${count:-0}"

        # Urgent flag anywhere under this workspace (0 if workspace missing)
        urgent="$(printf '%s' "$tree_json" | jq --argjson n "$n" '
            (.. | objects | select(.type=="workspace" and .num==$n)) as $ws
            | [$ws | .. | objects | select(.urgent==true)] | length
        ')"
        urgent="${urgent:-0}"

        # Decide icon + color (urgent does NOT override if focused)
        if [ "$n" -eq "$focused" ]; then
            icon="●"; fmt="%{T2}%{F#ffffff}"     # focused
        elif [ "$urgent" -gt 0 ]; then
            icon="◉"; fmt="%{T2}%{F#ff0f37}"     # urgent (orangish placeholder)
        elif [ "$count" -gt 0 ]; then
            icon="◉"; fmt="%{T2}%{F#ffffff}"     # occupied
        else
            icon="○"; fmt="%{T2}%{F#ffffff}"     # empty
        fi

        # --- Actions ---
        # Left-click: jump directly to this workspace
        printf "%s" "%{A1:i3-msg 'workspace number $n':}"
        # Scroll up: go to prev workspace number (wraps 1↔N)
        printf "%s" "%{A4:i3-msg 'workspace number $prev_ws':}"
        # Scroll down: go to next workspace number (wraps N↔1)
        printf "%s" "%{A5:i3-msg 'workspace number $next_ws':}"

        # --- Draw the icon with formatting ---
        printf "%s%s" "$fmt" "$icon"

        # --- Reset styles and close all three action blocks ---
        # %{-u}   : clear underline
        # %{B-}   : clear background
        # %{F-}   : clear foreground color
        # %{T-}   : reset font index
        # %{A}x3  : close A1, A4, A5 blocks
        printf "%s" "%{-u}%{B-}%{F-}%{T-}%{A}%{A}%{A} "
    done

    echo
}

# Print once, then update on changes
workspaces
i3-msg -t subscribe -m '["workspace","window"]' | while read -r _; do
    workspaces
done
