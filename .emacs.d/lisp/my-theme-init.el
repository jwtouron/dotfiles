(use-package arjen-grey-theme
  :if nil
  :init (load-theme 'arjen-grey t)
  :config (set-face-background hl-line-face "#2f4f4f"))

(use-package color-theme-sanityinc-tomorrow
  :if nil
  :init (load-theme 'sanityinc-tomorrow-night t))

(use-package gruber-darker-theme
  :if nil
  :init (load-theme 'gruber-darker t))

(use-package minimal-theme
  :if nil
  :init (load-theme 'minimal t)
  :config
  (progn
    (set-face-attribute 'region nil :background "grey40")
    (set-face-attribute 'font-lock-comment-face nil :foreground "grey32")
    (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "grey32")))

(use-package modus-themes
  :if nil
  :init (modus-themes-load-themes)
  :config (modus-themes-load-vivendi))

(use-package nord-theme
  :if nil
  :init (load-theme 'nord t))

(use-package spaceway-theme
  :ensure nil
  :init
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "lisp/spaceway"))
  (load-theme 'spaceway t))

(use-package tao-theme
  :if nil
  :custom (tao-theme-use-sepia nil)
  :init (load-theme 'tao-yin t))

(use-package tron-legacy-theme
  :if nil
  :init
  (setq tron-legacy-theme-vivid-cursor t
        tron-legacy-theme-softer-bg t)
  (load-theme 'tron-legacy t))

(use-package vscode-dark-plus-theme
  :if nil
  :init (load-theme 'vscode-dark-plus t))

(use-package warm-night-theme
  :if nil
  :init (load-theme 'warm-night t))

(use-package zenburn-theme
  :if nil
  :init (load-theme 'zenburn t))

(custom-set-faces
 '(show-paren-match ((t (:underline t :foreground nil :background nil)))))

(provide 'my-theme-init)
