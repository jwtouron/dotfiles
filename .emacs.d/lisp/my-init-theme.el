(defmacro use-themes (enabled &rest themes)
  (cl-labels ((apply-if-nil (theme)
                            (if (equal enabled (car theme))
                                theme
                              (cons (car theme)
                                    (append '(:if nil) (cdr theme))))))
    (cons 'progn
          (mapcar #'(lambda (theme)
                      (cons 'use-package (apply-if-nil theme)))
                  themes))))
(put 'use-themes 'lisp-indent-function 1)

(use-themes everforest
  (arjen-grey-theme
   :init (load-theme 'arjen-grey t)
   :config (set-face-background hl-line-face "#2f4f4f"))

  (atom-one-dark-theme
   :init (load-theme 'atom-one-dark t))

  (color-theme-sanityinc-tomorrow
   :init (load-theme 'sanityinc-tomorrow-night t))

  (doom-themes
   :init (load-theme 'doom-one t))

  (everforest
   :ensure nil
   :init
   (add-to-list 'custom-theme-load-path (concat user-emacs-directory "lisp/everforest-theme"))
   (load-theme 'everforest-hard-dark t)
   (set-face-attribute 'region nil :background "#3a454a"))

  (gruber-darker-theme
   :init (load-theme 'gruber-darker t))

  (minimal-theme
   :init (load-theme 'minimal t)
   :config
   (progn
     (set-face-attribute 'region nil :background "grey40")
     (set-face-attribute 'font-lock-comment-face nil :foreground "grey32")
     (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "grey32")))

  (modus-themes
   :init (modus-themes-load-themes)
   :config (modus-themes-load-vivendi))

  (nord-theme
   :init (load-theme 'nord t))

  (spaceway-theme
   :ensure nil
   :init
   (add-to-list 'custom-theme-load-path (concat user-emacs-directory "lisp/spaceway"))
   (load-theme 'spaceway t))

  (tao-theme
   :custom (tao-theme-use-sepia nil)
   :init (load-theme 'tao-yin t))

  (tron-legacy-theme
   :init
   (setq tron-legacy-theme-vivid-cursor t
         tron-legacy-theme-softer-bg t)
   (load-theme 'tron-legacy t))

  (vscode-dark-plus-theme
   :init (load-theme 'vscode-dark-plus t))

  (warm-night-theme
   :init (load-theme 'warm-night t))

  (zenburn-theme
   :init (load-theme 'zenburn t))
  )

(custom-set-faces
 '(show-paren-match ((t (:underline t :foreground nil :background nil)))))

(provide 'my-init-theme)
