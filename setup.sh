#!/usr/bin/env bash

set -euo pipefail

DOTFILES_DIR="$(dirname $(realpath $0))"

cd "$DOTFILES_DIR"

make_symlinks() {
    for src_file in $1; do
        # echo "$src_file"
        dst_file="$HOME/$src_file"
        src_file="$(realpath $src_file)"
        # dst_file="$HOME/$(basename $src_file)"
        # echo "$src_file => $dst_file"
        if [ -e "$dst_file" ] && ! [ -L "$dst_file" ]; then
            echo -e "\e[33mFile exists and is not a symlink: $dst_file\e[0m"
        else
            mkdir -p "$(dirname $dst_file)"; ln -sf "$src_file" "$dst_file"
        fi
    done
}

make_symlinks "$(find . -maxdepth 1 -mindepth 1 -type f -name .\*)"
make_symlinks "$(find .config -maxdepth 1 -mindepth 1 -name \*)"
make_symlinks "$(find .local/bin -maxdepth 1 -mindepth 1 -type f -name \*)"
make_symlinks "$(find .local/share -maxdepth 1 -mindepth 1 -name \*)"

