;;;; mode-line

(load-module "cpu")
(setf cpu:*cpu-modeline-fmt* "%c")

(load-module "mem")
;;(setf mem:*mem-modeline-fmt* "MEM: %b %p")

(setf *mode-line-foreground-color* "Gray75"
      *mode-line-pad-y* 5
      *mode-line-timeout* 1
      *time-modeline-string* "%a, %d %b %Y, %H:%M")

(setf *screen-mode-line-format*
      (list
       "%g^>"
       ;; '(:eval (async-run "curl -Ss \"https://wttr.in78261?0&T&Q\" | cut -c 16- | head -2"))
       ;; | xargs echo \" \"
       "%C"
       " | "
       "%M"
       " | "
       "%d"
       "      "))
;; (run-shell-command "curl -Ss \"https://wttr.in/78261?0&T&Q\" | cut -c 16- | head -2")

(loop for screen in *screen-list* do
      (loop for head in (screen-heads screen) do
            (enable-mode-line screen head t)))

(load-module "stumptray") ; (ql:quickload “xembed”)
(setf *tray-win-background* (nth 6 stumpwm:*colors*))
(stumptray::stumptray)
