#!/bin/sh

minimized_clients=$(
for client in `herbstclient list_clients`; do
    if [ "$(herbstclient get_attr clients.${client}.minimized)" = "true" ]; then
        echo "$(herbstclient get_attr clients.${client}.title) $client";
    fi;
done
)

if [ -z "$minimized_clients" ]; then
    exit
elif [ $(echo "$minimized_clients" | wc -l) -eq 1 ]; then
    herbstclient jumpto last-minimized
else
    preview='c={-1}; printf "title:      $(herbstclient get_attr clients.$c.title)\nclass:      $(herbstclient get_attr clients.$c.class)\npid:        $(herbstclient get_attr clients.$c.pid)\nfloating:   $(herbstclient get_attr clients.$c.floating)\nfullscreen: $(herbstclient get_attr clients.$c.fullscreen)\ninstance:   $(herbstclient get_attr clients.$c.title)"'
    client=$(echo "$minimized_clients" | fzfmenu --prompt='Unminimize: ' --no-sort --layout=reverse --preview="$preview" --preview-window=down)
    [ -z "$client" ] && exit
    echo "$client" | awk '{system("herbstclient jumpto "$NF)}'
fi
