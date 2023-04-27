;; -*- lexical-binding: t; -*-

(require 'url)


(defcustom weather-buffer-name "*weather*"
  "Name of the buffer used by `WEATHER'"
  :type 'string
  :group 'weather)

(defcustom weather-location nil
  "Location used by `WEATHER'"
  :type 'string
  :group 'weather)

;; TODO: Make it work with wget as a fallback.
;;;###autoload
(defun weather ()
  "Display the weather for `WEATHER-LOCATION'. Depends on curl."
  (interactive)
  (let ((weather-location (or weather-location "")))
    (switch-to-buffer weather-buffer-name)
    (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer weather-buffer-name)))
    (read-only-mode 0)
    (erase-buffer)
    (insert (shell-command-to-string (format "curl -s 'https://wttr.in/%s?u&T'" weather-location)))
    (read-only-mode 1)))

(provide 'weather)
