;; -*- lexical-binding: t; -*-

;;;; Initialization for various languages
;;;; Depends on use-package

;; C/C++

(add-hook 'c-mode-common-hook
          (lambda ()
            (electric-pair-local-mode 1)
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
  :hook (clojure-mode . paredit-mode))

(use-package cider)

;; emacs lisp

(use-package elisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . flymake-mode)
         (emacs-lisp-mode . paredit-mode)))

;; go

(use-package go-mode
  :hook ((go-mode . electric-pair-local-mode)))

;; haskell

(use-package haskell-mode
  :hook ((haskell-mode . electric-pair-local-mode)
         (haskell-mode . interactive-haskell-mode)
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

(add-hook 'lisp-mode-hook #'paredit-mode)

(use-package sly
  :custom (sly-default-lisp 'roswell)
  :hook (sly-mode . paredit-mode)
  :init
  (let ((ros-config (concat user-emacs-directory
                            "ros-conf.lisp")))
    (setq sly-lisp-implementations
          `((sbcl    ("sbcl"))
            (ccl     ("ccl"))
            (clisp   ("clisp"))
            (ecl     ("ecl"))
            (roswell ("ros" "-Q" "-l" ,ros-config "run"))
            ;; (qlot ("qlot" "exec" "ros" "run" "-S" "."))
            ))))

(use-package sly-asdf
  :after sly
  :init (add-to-list 'sly-contribs 'sly-asdf 'append))

(use-package sly-quicklisp
  :after sly)

;; lua

(use-package lua-mode
  :custom ((lua-indent-level 4)))

;; markdown

(use-package markdown-mode)

;; nim

(use-package nim-mode
  :init
  (defun my--init-nim-mode ()
    (nimsuggest-mode 1)
    (electric-pair-local-mode 1))
  (add-hook 'nim-mode-hook 'my--init-nim-mode))

;; Rust

(use-package rust-mode
  :hook (rust-mode . my--init-rust-mode)
  :bind (:map rust-mode-map
              ("C-c m r" . 'rust-run)
              ("C-c m c" . 'rust-compile)
              ("C-c m t" . 'rust-test)
              ("C-c m k" . 'rust-check))
  :config
  (defun my--init-rust-mode ()
    (electric-pair-local-mode)))

(use-package cargo-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; xml

(use-package nxml-mode
  :ensure nil
  :hook ((nxml-mode . electric-pair-local-mode)))

;; python

(use-package python
  :ensure nil
  :hook ((python-mode . electric-pair-local-mode))
  :config
  (if (executable-find "ipython3")
      (setq python-shell-interpreter "ipython3"
            python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
    (setq python-interpreter "python3")))


;; yaml

(use-package yaml-mode)

(provide 'my-init-langs)
