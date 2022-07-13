;;;; Startup Programs

(run-shell-command "nm-applet &")
(run-shell-command "nitrogen --restore &")
(run-shell-command "picom &")
(run-shell-command "pgrep cbatticon || cbatticon &")
;; (run-shell-command "pgrep cbatticon || [ \"$(cbatticon -p | wc -l)\" -gt 1 ] && cbatticon &")
(run-shell-command "pgrep udiskie || udiskie; udiskie -s &")

(run-shell-command "pgrep polybar || for m in $(polybar --list-monitors | cut -d\":\" -f1); do MONITOR=$m polybar --reload stumpwm; done")
