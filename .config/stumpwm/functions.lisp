;;;; Command and Functions

(defun executable-p (name)
  "Tell if given executable is present in PATH."
  (let ((which-out (string-trim '(#\  #\linefeed) (run-shell-command (concat "which " name) t))))
    (unless (string-equal "" which-out) which-out)))

(defcommand browser ()
  nil
  (dolist (browser '("brave"
                     "brave-browser"
                     "firefox"
                     "chromium"
                     "chromium-browser"
                     "chrome"
                     "google-chrome"))
    (when (executable-p browser)
      (uiop:launch-program browser)
      (return))))

(defcommand terminal ()
  nil
  (dolist (terminal '("st" "xterm"))
    (when (executable-p terminal)
      (uiop:launch-program terminal)
      (return))))

(defcommand powermenu ()
  nil
  (run-shell-command "~/.config/stumpwm/powermenu.sh"))

(defun swap-groups (group1 group2)
  (rotatef (slot-value group1 'number) (slot-value group2 'number)))

(defun move-group-forward (&optional (group (current-group)))
  (swap-groups group (next-group group (sort-groups (current-screen)))))

(defun move-group-backward (&optional (group (current-group)))
  (swap-groups group (next-group group (reverse (sort-groups (current-screen))))))

(define-stumpwm-command "gforward" ()
  (move-group-forward)
  (echo-groups (current-screen) *group-format*))

(define-stumpwm-command "gbackward" ()
  (move-group-backward)
  (echo-groups (current-screen) *group-format*))

(defcommand gselect-new (name)
  ((:string "Group name: "))
  (when (not (stumpwm::select-group (current-screen) name))
    (gnewbg name))
  (gselect name))

(defcommand gmove-new (to-group)
  ((:string "Group name: "))
  (let ((current (current-window)))
    (when (not (stumpwm::select-group (current-screen) to-group))
      (gnewbg to-group))
    (move-window-to-group current to-group)))
