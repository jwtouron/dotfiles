#!/bin/sh

set =eu

cd "$(dirname "$(realpath "$0")")"

theme="$(find themes -maxdepth 1 -type f | fzf)"

[ -z "$theme" ] && exit 0

ln -sf "$PWD/$theme" "$PWD/current-theme.conf"
