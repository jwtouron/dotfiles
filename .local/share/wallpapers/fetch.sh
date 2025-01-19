#!/usr/bin/env bash

set -euxo pipefail

if ! command -v git > /dev/null; then
    echo "Error: Missing git"
    exit 1
fi

if ! command -v rclone > /dev/null; then
    echo "Error: Missing rclone"
    exit 1
fi

if [ ! -d ~/.local/share/wallpapers/jped ]; then
    git clone --depth 1 https://gitlab.com/jped/wallpapers ~/.local/share/wallpapers/jped
fi

if [ ! -d ~/.local/share/wallpapers/dwt1 ]; then
    git clone --depth 1 https://gitlab.com/dwt1/wallpapers ~/.local/share/wallpapers/dwt1
fi

if [ ! -d ~/.local/share/wallpapers/murzchnvok/ ]; then
    if ! rclone config show | grep -q '\[gdrive\]'; then
        rclone config create gdrive drive
    fi

    rclone copy --progress gdrive,root_folder_id=1o1qjRgkJtnF_8uGB1z6MRsQUjWinHUsw: ~/.local/share/wallpapers/murzchnvok/
fi
