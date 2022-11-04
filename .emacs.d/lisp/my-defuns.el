;; -*- lexical-binding: t; -*-

(require 'project)

;;;###autoload
(defun my-set-font ()
  "Interactively set the font"
  (interactive)
  (let ((font-name (completing-read "Font name: " (sort (delete-dups (font-family-list)) 'string-lessp)))
        (font-size (read-string "Font size: ")))
    (customize-save-variable 'my-font `(,font-name . ,(string-to-number font-size)))))

;;;###autoload
(defun my-set-theme (theme)
  (interactive
   (list
    (intern
     (completing-read "Load theme: "
                      (mapcar #'symbol-name (custom-available-themes))))))
  ;; This code copied from `consult-theme'
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm))
      (customize-save-variable 'my-theme `,theme))))

;;;###autoload
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))
(defalias 'nd 'narrow-or-widen-dwim)

;;;###autoload
(defun align-non-space (beg end)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)\\S-+" 1 1 t))

;;;###autoload
(defun my-test-native-compilation ()
  "Tests if native compilation is available."
  (interactive)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (message "Native compilation is available")
    (message "Native complation is *not* available")))

;;;###autoload
(defun my-test-fast-json ()
  "Tests if emacs has fast JSON support."
  (interactive)
  (if (functionp 'json-serialize)
      (message "Native JSON is available")
    (message "Native JSON is *not* available")))

;;;###autoload
(defun my-split-window-below (arg)
  (interactive "P")
  (let ((new-win (split-window-below)))
    (when arg
      (select-window new-win)
      (counsel-find-file))))

;;;###autoload
(defun my-split-window-right (arg)
  (interactive "P")
  (let ((new-win (split-window-right)))
    (when arg
      (select-window new-win)
      (counsel-find-file))))

;;;###autoload
(defun advice-add-repeat-mode (func &rest bindings)
  (advice-add func :after
              (lambda (&rest _)
                (set-transient-map
                 (let ((map (make-sparse-keymap)))
                   (dolist (binding bindings)
                     (define-key map (kbd (car binding)) (cdr binding)))
                   map)))))

;;;###autoload
(defun my-insert-date ()
  (interactive)
  (insert (string-trim (shell-command-to-string "date +\"%Y-%m-%d\""))))

;;;###autoload
(defun my-insert-start-of-week (&optional date)
  (interactive)
  (let* ((date (or date ""))
         (shell-command (string-join `(,(format "start=$(date -d \"%s\" +%%s)" date)
                                       "consider=\"$start\""
                                       "dow=$(date -d \"@$consider\" +%A)"
                                       "while [ \"$dow\" != \"Sunday\" ]; do"
                                       "  let \"consider=$consider - 86400\""
                                       "  dow=$(date -d \"@$consider\" +%A)"
                                       "done"
                                       "date -d \"@$consider\" +%Y-%m-%d")
                                     "; ")))
    (insert (string-trim-right (shell-command-to-string shell-command)))))

(defun my-switch-buffer (switch-fun &optional arg)
  (let ((current-project (project-current)))
    (if (or arg
            (not current-project))
        (funcall switch-fun)
      (let* ((project-buffers (when current-project (project--buffer-list current-project)))
             (switch-to-prev-buffer-skip
              (lambda (_ buffer _)
                (not (memq buffer project-buffers)))))
        (funcall switch-fun)))))

;;;###autoload
(defun my-next-buffer (&optional arg)
  (interactive "P")
  (my-switch-buffer #'next-buffer arg))

;;;###autoload
(defun my-previous-buffer (&optional arg)
  (interactive "P")
  (my-switch-buffer #'previous-buffer arg))

;;;###autoload
(defun my-upgrade-packages ()
  "Upgrade packages installed through package.el and Quelpa."
  (interactive)
  (require 'cl)
  (flet ((pop-to-buffer-same-window (buffer &optional norecord)))
    (let ((package-menu-async nil))
      (list-packages)
      (with-current-buffer "*Packages*"
        (package-menu-mark-upgrades)
        (ignore-error 'user-error (package-menu-execute t)))))
  (when (fboundp 'quelpa-upgrade-all)
    (quelpa-upgrade-all)))

(provide 'my-defuns)
