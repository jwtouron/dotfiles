zcd() {
    cd $(fd -H -t d . $1 | fzf)
}

zkill() {
    process=$(ps -e -o pid=,args= | fzf -m | awk '{print $1}')
    [ -z "$process" ] && return
    echo "$process" | xargs kill "$@"
}

zmount() {
    header=$(lsblk -l | head -n 1)
    dev=$(lsblk -lp | tail -n+2 | awk '/loop/ {} { print $1 }' | fzf --prompt='Device: ' --preview="echo $header; lsblk -lp | grep '^'{}'\b'")

    [ -z "$dev" ] && return

    mount_point=$(echo "/media/$(whoami)\n/media/tmp\nother" | fzf --prompt='Mount point: ')

    if [ "$mount_point" = "other" ]; then
        echo -n "Mount point: "
        read mount_point || return
    fi

    [ -z "$mount_point" ] && return

    sudo mkdir -p "$mount_point" && sudo mount "$dev" "$mount_point"
}

zpkginstall() (
    if command -v pacman >/dev/null; then
        pkgs=$(pacman -Ssq . | fzf -m)
        [ -n "$pkgs" ] && sudo pacman --needed -S $(echo "$pkgs" | xargs)
    elif command -v apt >/dev/null; then
        pkgs=$(apt search . 2>/dev/null | awk '/^[^ ]/ { sub(/\/.*/, ""); print $0 } {}' | fzf -m)
        [ -n "$pkgs" ] && echo "$pkgs" | xargs sudo apt install
    fi
)

zps() {
    ps aux | fzf
}

zumount() {
    header=$(df -h | head -n 1)

    mount_point=$(df -h | tail -n+2 | awk '{print $NF}' | fzf --preview="echo $header; df -h | grep ' {}$'" --preview-window=60%)

    [ -z "$mount_point" ] && return

    sudo umount "$mount_point"
}

zvim() (
    files=$(fd -H -t f . $1 | fzf -m)
    [ -n "$files" ] && echo "$files" | tr '\n' '\0' | xargs -o -0 vim --
)
