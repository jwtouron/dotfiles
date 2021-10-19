#!/bin/sh

i3-nagbar -t warning -m 'Goodbye!' \
    -b '(S)hutdown' 'shutdown now' \
    -b 'S(u)spend' 'systemctl suspend' \
    -b '(R)eboot' reboot \
    -B 'Loc(k)' i3lock \
    -b '(L)ogout' 'i3-msg exit'

