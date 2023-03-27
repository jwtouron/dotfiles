;; -*- lexical-binding: t; -*-

;;;; Initialization for various languages
;;;; Depends on use-package

(add-hook 'prog-mode-hook #'electric-pair-local-mode)

;; C/C++

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "TAB")  'indent-for-tab-command)))
(c-add-style "my-c-style"
             '("stroustrup"
               (c-offsets-alist
                (case-label . +))))
(setq-default c-default-style
              '((java-mode . "java")
                (awk-mode . "awk")
                (other . "my-c-style")))

;; Clojure

(use-package clojure-mode
  :hook (clojure-mode . my--init-clojure-mode)
  :config
  (defun my--init-clojure-mode ()
    (electric-pair-local-mode -1)
    (paredit-mode)))

(use-package cider)

;; emacs lisp

(use-package elisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . my--init-elisp-mode)
  :config
  (defun my--init-elisp-mode ()
    ;; (electric-pair-local-mode -1)
    (flymake-mode)
    ;; (paredit-mode)
    ))

;; go

(use-package go-mode)

;; groovy

(use-package groovy-mode)

;; haskell

(use-package haskell-mode
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . my-haskell-minor-mode))
  :custom ((haskell-process-use-presentation-mode t))
  :init
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c m e") 'my-haskell-eval)
    (define-key keymap (kbd "C-c m h") 'haskell-hoogle)
    (define-key keymap (kbd "C-c m r") 'haskell-process-restart)
    (define-key keymap (kbd "C-c m t") 'my-haskell-eval-type)
    (define-minor-mode my-haskell-minor-mode
      "Sets up my settings for Haskell"
      :keymap keymap))
  :config
  (defun my-haskell-eval-type ()
    (interactive)
    (let ((line (if (region-active-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (symbol-at-point)) ))
      (haskell-process-show-repl-response (format ":t %s" line))))
  (defun my-haskell-eval ()
    (interactive)
    (let ((line (if (region-active-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (string-trim-left (buffer-substring-no-properties (line-beginning-position)
                                                                    (line-end-position))
                                    " *-- *"))))
      (haskell-process-show-repl-response line))))

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :hook (haskell-mode . dante-mode))

;; lisp

(use-package lisp-mode
  :ensure nil
  :hook (lisp-mode . my--init-lisp-mode)
  :config
  (defun my--init-lisp-mode ()
    (electric-pair-local-mode -1)
    (paredit-mode)))

(use-package slime
  :hook (slime-repl-mode . paredit-mode)
  :config
  (cond
   ((executable-find "sbcl")
    (let ((quicklisp-path (expand-file-name "~/quicklisp/setup.lisp")))
      (if (file-exists-p quicklisp-path)
          (setq inferior-lisp-program (string-join (list "sbcl --load " quicklisp-path)))
        (setq inferior-lisp-program "sbcl"))))
   ((executable-find "ros")
    (setq inferior-lisp-program "ros -Q run")))
  (slime-setup '(slime-fancy slime-quicklisp slime-asdf))
  (setq slime-completion-at-point-functions
        (list #'slime-simple-completion-at-point)))

;; (use-package slime-company
;;   :after (slime company)
;;   :config (setq slime-company-completion 'fuzzy
;;                 slime-company-after-completion 'slime-company-just-one-space))

;; (use-package sly
;;   :hook (sly-mode . paredit-mode)
;;   :init
;;   (setq sly-lisp-implementations
;;         '((roswell  ("ros" "-Q" "run"))
;;           (sbcl ("sbcl"))))
;;   (setq sly-default-lisp (cond
;;                           ((executable-find "ros") 'roswell
;;                            (executable-find "sbcl") 'sbcl))))

;; (use-package sly-asdf
;;   :after sly
;;   :config (add-to-list 'sly-contribs 'sly-asdf 'append))

;; (use-package sly-quicklisp
;;   :after sly)

;; lua

(use-package lua-mode
  :custom ((lua-indent-level 4)))

;; markdown

(use-package markdown-mode)

;; nim

(use-package nim-mode
  :hook (nim-mode . nimsuggest-mode))

;; Rust

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c m r" . 'rust-run)
              ("C-c m c" . 'rust-compile)
              ("C-c m t" . 'rust-test)
              ("C-c m k" . 'rust-check)))

(use-package cargo-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; python

(use-package python
  :ensure nil
  :config
  (if (executable-find "ipython3")
      (setq python-shell-interpreter "ipython3"
            python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
    (setq python-interpreter "python3")))

;; web

(use-package vue-mode)

;; yaml

(use-package yaml-mode)

(provide 'my-init-langs)
