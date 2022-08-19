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

(defmacro advice-add-theme (theme1 &rest body)
  `(advice-add 'load-theme
               :after
               (lambda (theme2 &rest _ignored)
                 (when (eq ,theme1 theme2)
                   ,@body))))
(put 'advice-add-theme 'lisp-indent-function 1)

(use-package arjen-grey-theme
  :init
  (advice-add-theme 'arjen-grey
    (set-face-background hl-line-face "#2f4f4f")))

(use-package atom-one-dark-theme)

(use-package color-theme-sanityinc-tomorrow)

(use-package doom-themes)

(use-package ef-themes)

(use-package everforest
  :ensure nil
  :init
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "lisp/everforest-theme"))
  (advice-add-theme 'everforest-hard-dark
    (set-face-attribute 'region nil :background "#3a454a")))

(use-package gruber-darker-theme)

(use-package minimal-theme
  :init
  (advice-add-theme 'minimal
    (set-face-attribute 'region nil :background "grey40")
    (set-face-attribute 'font-lock-comment-face nil :foreground "grey32")
    (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "grey32")))

(use-package modus-themes
  :init (modus-themes-load-themes))

(use-package nord-theme)

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
