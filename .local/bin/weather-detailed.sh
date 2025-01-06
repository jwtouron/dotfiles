#!/bin/sh

set -eu

if ! command -v jq >/dev/null; then
    echo "ERROR: jq is required" 1>&2 && exit 1
fi

if ! command -v wego >/dev/null; then
    echo "ERROR: wego is required" 1>&2 && exit 1
fi

LOC="$(curl -s ipinfo.io | jq -r '.loc')"

wego -d 10 "$LOC"
