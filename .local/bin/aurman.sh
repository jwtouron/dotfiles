#!/bin/sh

set -e

[ -z "$1" ] && echo "Missing name of package as argument" && exit 1

[ ! -d "$HOME/projects" ] && mkdir -p "$HOME/projects"

cd "$HOME/projects"

if [ -d "$PWD/$1" ]; then
    cd "$1"
    git pull --rebase --recurse-submodules
else
    git clone "https://aur.archlinux.org/$1.git"
    cd "$1"
fi

makepkg --clean --install --rmdeps --syncdeps
