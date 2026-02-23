#!/bin/sh
set -eu

SRC="$HOME/.config/niri/waypaper"

systemctl --user link \
  "$SRC/waypaper.service" \
  "$SRC/waypaper.timer"

systemctl --user daemon-reload
systemctl --user enable waypaper.timer
systemctl --user start waypaper.service
