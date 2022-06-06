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
  :init (load-theme 'minimal t))

(use-package modus-themes
  :if nil
  :init (modus-themes-load-themes)
  :config (modus-themes-load-vivendi))

(use-package nord-theme
  :if nil
  :init (load-theme 'nord t))

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
