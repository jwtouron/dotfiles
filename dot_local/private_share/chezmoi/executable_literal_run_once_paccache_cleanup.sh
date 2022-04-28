#!/bin/bash

command -v pacman >/dev/null || exit 0

sudo mkdir -p /etc/pacman.d/hooks || exit 1

cat << EOF | sudo tee /etc/pacman.d/hooks/paccache-cleanup.hook
[Trigger]
Operation = Upgrade
Type = Package
Target = *

[Action]
Description = Cleaning pacman cache...
When = PostTransaction
Exec = /usr/bin/paccache -r
EOF
