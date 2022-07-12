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
