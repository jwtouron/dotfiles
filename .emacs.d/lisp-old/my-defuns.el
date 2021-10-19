;; -*- lexical-binding: t; -*-

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

(defun my-set-font (&optional force)
  "Automatically set the font."
  (interactive "P")
  (let ((file-path "~/.emacs.d/font"))
    (when (or (not (file-exists-p file-path)) force)
      (let ((font-name (ivy-read "Font name: " (sort (delete-dups (font-family-list)) 'string-lessp)))
            (font-size (read-string "Font size: ")))
        (write-region (concat font-name ":" font-size) nil file-path)))
    (let* ((font-spec (split-string (with-temp-buffer
                                      (insert-file-contents file-path)
                                      (buffer-string))
                                    ":"))
           (font-name (car font-spec))
           (font-size (string-to-number (cadr font-spec))))
      (set-face-attribute 'default nil :font font-name :height font-size)
      (message (format "Font set to %s (%d)" font-name font-size)))))

(defun daedreth/kill-inner-word ()
  "Kills the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

(defun daedreth/copy-whole-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (kill-word 1)
    (yank)))

(defun daedreth/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))

(provide 'my-defuns)
