;; -*- lexical-binding: t; -*-

;;;###autoload
(defun my-set-font ()
  "Interactively set the font"
  (interactive)
  (let ((font-name (ivy-read "Font name: " (sort (delete-dups (font-family-list)) 'string-lessp)))
        (font-size (read-string "Font size: ")))
    (customize-save-variable 'my-font `(,font-name . ,(string-to-number font-size)))))

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

(defconst my--rg-grep-command
  "find '%s' -type f %s -exec rg --color never -n --no-heading -e '%s' \"{}\" +")
(defconst my--grep-grep-command
  "find '%s' -type f %s -exec grep -E --color=never -nH --null -e '%s' \"{}\" +")

(defvar my-grep-command-history
  (let ((grep-command (if (executable-find "rg")
                          my--rg-grep-command
                        my--grep-grep-command)))
    (list (format grep-command "." "" "."))))

(defun my--grep-update-command-history (cmd)
  (when (not (string-equal cmd (car my-grep-command-history)))
    (setq my-grep-command-history (seq-take (cons cmd my-grep-command-history) 10))))

;;;###autoload
(defun my-grep (arg)
  (interactive "P")
  (let ((grep-command
         (if arg
             (read-string "grep command: " (car my-grep-command-history) 'my-grep-command-history)
           (let* ((search-str (read-string "Search for: "))
                  (default-glob-str (if-let ((bfn (buffer-file-name))
                                             (ext (file-name-extension bfn)))
                                        (format "*.%s" ext)
                                      ""))
                  (glob-str (read-string "Globs (comma separated): " default-glob-str))
                  (globs (if (string-equal glob-str "")
                             nil
                           (split-string glob-str  ",")))
                  (dir (read-directory-name "Start directory: " (projectile-project-root)))
                  (find-glob-str (if globs
                                     (format "\\( -iname '%s' \\)" (string-join globs "' -o -iname '"))
                                   ""))
                  (grep-command (if (executable-find "rg")
                                    my--rg-grep-command
                                  my--grep-grep-command)))
             (format grep-command dir find-glob-str search-str)))))
    (my--grep-update-command-history grep-command)
    (grep-find grep-command)))

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

(provide 'my-defuns)
