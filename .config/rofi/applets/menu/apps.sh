#!/usr/bin/env bash

## Author  : Aditya Shakya
## Mail    : adi1090x@gmail.com
## Github  : @adi1090x
## Twitter : @adi1090x

style="$($HOME/.config/rofi/applets/menu/style.sh)"

dir="$HOME/.config/rofi/applets/menu/configs/$style"
rofi_command="rofi -theme $dir/apps.rasi"

# Links
terminal=""
browser=""
editor=""
files=""

# Error msg
msg() {
	rofi -theme "$HOME/.config/rofi/applets/styles/message.rasi" -e "$1"
}

# Variable passed to rofi
options="$terminal\n$browser\n$editor\n$files"

chosen="$(echo -e "$options" | $rofi_command -p "Most Used" -dmenu -selected-row 0)"
case $chosen in
    $terminal)
		if [[ -f /usr/bin/st ]]; then
		    st &
		elif [[ -f /usr/bin/xterm ]]; then
		    xterm &
		elif [[ -f /usr/bin/kitty ]]; then
		    kitty &
		elif [[ -f /usr/bin/termite ]]; then
		    termite &
		else
		    msg "No suitable terminal found!"
		fi
	;;
    $files)
		if [[ -f /usr/bin/pcmanfm ]]; then
			pcmanfm &
		elif [[ -f /usr/bin/thunar ]]; then
			thunar &
		else
			msg "No suitable file manager found!"
		fi
        ;;
    $editor)
		if [[ -f /usr/bin/emacs ]]; then
			emacs &
		elif [[ -f /usr/bin/geany ]]; then
			geany &
		elif [[ -f /usr/bin/leafpad ]]; then
			leafpad &
		elif [[ -f /usr/bin/mousepad ]]; then
			mousepad &
		elif [[ -f /usr/bin/code ]]; then
			code &
		else
			msg "No suitable text editor found!"
		fi
        ;;
    $browser)
		if [[ -f /usr/bin/brave ]]; then
			brave &
		elif [[ -f /usr/bin/firefox ]]; then
			firefox &
		elif [[ -f /usr/bin/chromium ]]; then
			chromium &
		elif [[ -f /usr/bin/midori ]]; then
			midori &
		else
			msg "No suitable web browser found!"
		fi
        ;;
esac
