#!/bin/sh

while :; do
    installed=$(pacman -Q | wc -l)
    available=$(checkupdates --nocolor | wc -l)
    threshold=10
    printf "%f %d %f" $installed $available $threshold | \
        awk '{
                if ( $2 >= ($1 * $3) / 100 ) {
                    system("notify-send -u critical \"Updates Available\" \"There are " $2 " updates available\"")
                }
             }'
    sleep 86400
done
