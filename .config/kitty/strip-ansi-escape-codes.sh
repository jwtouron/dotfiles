#!/bin/sh

if command -v gcc >/dev/null; then
    if ! test -x "$HOME/.config/kitty/strip-ansi-escape-codes"; then
        gcc -std=c99 -O2 -Wall -Wextra -o "$HOME/.config/kitty/strip-ansi-escape-codes" "$HOME/.config/kitty/strip-ansi-escape-codes.c"
    fi
    "$HOME/.config/kitty/strip-ansi-escape-codes"
elif command -v perl >/dev/null; then
    perl -pe 's/\x1b(\[[0-9;:]*m|]8;;|][0-9;:]*[AC]|[\\])//g'
elif command -v sed >/dev/null; then
    sed -e 's/\x1b\(\[[0-9;:]*m\|]8;;\|][0-9;:]*[AC]\|\\\)//g'
fi
