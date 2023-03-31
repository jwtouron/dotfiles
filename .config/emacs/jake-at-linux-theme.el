;;; -*- lexical-binding: t -*-

;;; For development:
;;; (local-set-key (kbd "<f5>") (lambda () (interactive) (save-buffer) (load-theme 'jake-at-linux t)))

(require 'autothemer)

(autothemer-deftheme jake-at-linux
  "Kitty terminal theme used by Jake@Linux"
  ((((class color) (min-colors #xFFFFFF)))
   (foreground "#c5c5be")
   (background "#1b1d1e")
   (cursor     "#c5c5be")

   (dark-purple  "#6e378a")
   (light-purple "#884fa5")
   (blue         "#5f87af")
   (dark-teal    "#007687")
   (light-teal   "#477d8f")

   (black "#1b1d1e")
   (white "#dadad5")

   (grey00 "#2f3234") ; (let ((mul 1.75)) (format "%2x%2x%2x" (* mul #x1b) (* mul #x1d) (* mul #x1e)))
   (grey01 "#505354")
   (grey02 "#615f5e")
   (grey03 "#737271")
   ;; (grey04 "#737074")
   (grey05 "#909495")
   ;; (grey06 "#9a999d")
   (grey07 "#a2a2a5")
   (grey08 "#c5c5be"))

  ((default                       (:foreground foreground :background background))
   ;; built-in
   (cursor                        (:background cursor))
   (font-lock-comment-face        (:foreground grey02 :slant 'italic))
   (font-lock-constant-face       (:foreground dark-teal))
   (font-lock-function-name-face  (:foreground light-teal))
   (font-lock-keyword-face        (:foreground light-purple))
   (font-lock-string-face         (:foreground blue))
   (font-lock-type-face           (:foreground light-teal))
   (font-lock-variable-name-face  (:foreground foreground))
   (hi-line                       (:background grey00))
   (highlight                     (:background grey00))
   (minibuffer-prompt             (:foreground light-teal))
   (mode-line                     (:background grey00 :box (:line-width 1 :color nil)))
   (mode-line-inactive            (:foreground grey07 :background grey01 :box (:line-width 1 :color nil)))
   (region                        (:background grey01))
   ;; diredp
   (diredp-date-time              (:foreground light-purple))
   (diredp-dir-heading            (:foreground light-purple))
   (diredp-dir-name               (:foreground blue))
   (diredp-file-name              (:foreground foreground))
   (diredp-file-suffix            (:foreground light-teal))
   (diredp-number                 (:foreground light-teal))
   ;; magit
   (magit-branch-local            (:foreground light-teal))
   (magit-branch-remote           (:foreground dark-teal))
   (magit-section-heading         (:foreground light-purple))
   (magit-section-highlight       (:foreground blue))
   ;; org
   (org-level-1                   (:foreground light-teal))
   (org-level-2                   (:foreground dark-teal))
   (org-level-3                   (:foreground light-purple))
   (org-level-4                   (:foreground dark-purple))
   (org-link                      (:foreground blue :underline t))
   ))
