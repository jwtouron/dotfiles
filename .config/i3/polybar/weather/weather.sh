#!/bin/sh

cd "$(dirname "$(realpath "$0")")"

[ -x ./weather ] || go build -o weather . || exit 1

./weather "$@"
