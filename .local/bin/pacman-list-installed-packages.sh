#!/bin/sh

pacman -Qie | \
    awk -F ' : '  '/^Name/ { name = $2 }
                   /^Install Date/ {
                       system("(echo " name "; date -d \"" $2 "\" +%s; echo " $2 ") | while read l; do echo -n \"$l : \"; done; echo")
                   }' | \
    sort -r -k 3 | \
    awk -F ' : ' '{ system("printf \"%-50s%s\n\" " $1 " \"" $3 "\"") }' | \
    fzf --multi --reverse --header=Packages --preview='pacman -Qie {1}' --preview-window=bottom


