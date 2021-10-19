;; -*- lexical-binding: t; -*-

(defconst my-theme-packages
  '(alect-themes
    arjen-grey-theme
    atom-dark-theme
    atom-one-dark-theme
    birds-of-paradise-plus-theme
    bubbleberry-theme
    chocolate-theme
    color-theme-sanityinc-tomorrow
    creamsody-theme
    darkburn-theme
    doom-themes
    dracula-theme
    gandalf-theme
    grayscale-theme
    gruvbox-theme
    heroku-theme
    inkpot-theme
    kaolin-themes
    minimal-theme
    modus-vivendi-theme
    molokai-theme
    monochrome-theme
    monokai-theme
    noctilux-theme
    nord-theme
    nova-theme
    planet-theme
    solarized-theme
    spacemacs-theme
    sublime-themes
    tangotango-theme
    tao-theme
    ubuntu-theme
    warm-night-theme
    zenburn-theme))

(dolist (package my-theme-packages)
  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))

(defun my-underline-match-paren (&optional theme)
  (interactive "sSymbol: ")
  (if theme
      (custom-theme-set-faces
       theme
       `(show-paren-match ((t (:underline t :foreground nil :background nil)))))
    (custom-set-faces
     '(show-paren-match ((t (:underline t :foreground nil :background nil)))))))

;; (load-theme 'doom-one t)
(load-theme 'modus-vivendi t)
(my-underline-match-paren)
;; (progn
;;   (set-face-background 'highlight nil)
;;   (set-face-foreground 'highlight nil)
;;   (set-face-underline-p 'highlight t))
;;(my-underline-match-paren)
;;(my-underline-match-paren 'grayscale)

(provide 'my-init-theme)
