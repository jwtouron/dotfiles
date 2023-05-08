(defmacro advice-add-theme (theme1 &rest body)
  `(advice-add 'enable-theme
               :after
               (lambda (theme2 &rest _ignored)
                 (when (eq ',theme1 theme2)
                   ,@body))))
(put 'advice-add-theme 'lisp-indent-function 1)

(use-package almost-mono-themes)

(use-package autothemer)

(use-package arjen-grey-theme
  :config
  (advice-add-theme arjen-grey
    (set-face-attribute 'highlight nil :background "#2f4f4f")))

(use-package atom-one-dark-theme)

(use-package base16-theme)

(use-package color-theme-sanityinc-tomorrow)

(use-package cyberpunk-theme)

(use-package doom-themes
  :config
  (advice-add-theme doom-opera
    (set-face-attribute 'default nil :background "#222224")
    (set-face-attribute 'hl-line nil :background "#323334")
    (set-face-attribute 'region  nil :background "#507681")))

(use-package ef-themes)

(use-package everforest-theme
  :ensure nil
  :init
  (push (expand-file-name (concat user-emacs-directory "lisp/everforest-theme")) custom-theme-load-path)
  :config
  (advice-add-theme everforest-hard-dark
    (set-face-attribute 'region nil :background "#3a454a")))

(use-package gruber-darker-theme)

(use-package minimal-theme
  :config
  (advice-add-theme minimal
    (set-face-attribute 'region nil :background "grey40")
    (set-face-attribute 'font-lock-comment-face nil :foreground "grey32")
    (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "grey32")))

(use-package modus-themes)

(use-package nord-theme)

(use-package nordic-night-theme)

(use-package spaceway-theme
  :ensure nil
  :init
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "lisp/spaceway")))

(use-package tao-theme
  :custom (tao-theme-use-sepia nil))

(use-package tron-legacy-theme
  :config
  (setq tron-legacy-theme-vivid-cursor t
        tron-legacy-theme-softer-bg t))

(use-package twilight-theme
  :config
  (advice-add-theme twilight
    (set-face-attribute 'highlight nil :background "#1d1d1d")))

(use-package vscode-dark-plus-theme)

(use-package warm-night-theme)

(use-package zenburn-theme)

(advice-add 'load-theme
            :after
            (lambda (_ignored1 &rest _ignored2)
              (custom-set-faces
               '(show-paren-match ((t (:underline t :foreground nil :background nil)))))))

(my-set-theme my-theme)

;; (custom-set-faces
;;  '(show-paren-match ((t (:underline t :foreground nil :background nil)))))

(provide 'my-init-theme)
