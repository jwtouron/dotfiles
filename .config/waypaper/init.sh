#!/bin/sh

set -eu

SRC="$HOME/.config/waypaper"

systemctl --user link \
  "$SRC/waypaper.service" \
  "$SRC/waypaper.timer"

systemctl --user daemon-reload
systemctl --user start waypaper.timer
systemctl --user start waypaper.service
