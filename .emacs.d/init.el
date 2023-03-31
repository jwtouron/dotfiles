;; -*- lexical-binding: t; -*-

;;; bootstrapping

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; use-package
(unless (require 'use-package nil t)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package)
  (require 'use-package))

(setq use-package-always-ensure t
      use-package-always-defer t)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; packages

(use-package gnu-elpa-keyring-update :demand t)

(require 'my-defuns)
(require 'my-defcustoms)
(require 'my-init-emacs)
;; (require 'my-keybindings)
(require 'my-init-cemov)

(use-package ace-window
  :bind ("C-x o" . 'ace-window))

(use-package avy
  :bind ("C-c j" . 'avy-goto-char-timer))

(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345)
         ("M-/" . cape-dabbrev))
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; an advanced Ctags (or actually, readtags) frontend
;; https://github.com/universal-ctags/citre
(use-package citre)

(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))

(use-package corfu
  :defer 1
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-x x r" . crux-rename-file-and-buffer)))

;; Run devdocs-install
(use-package devdocs
  :bind ("C-h D" . devdocs-lookup))

(use-package diminish)

(use-package dot-mode
  :if nil
  :diminish 'dot-mode
  :init (global-dot-mode t)
  :config (define-key dot-mode-map (kbd "C-M-.") nil))

(use-package dumb-jump
  :bind-keymap ("C-M-j" . my--dumb-jump-mode-map)
  :bind (:map my--dumb-jump-mode-map
              ("j" . dumb-jump-go)
              ("o" . dumb-jump-go-other-window)
              ("e" . dumb-jump-go-prefer-external)
              ("x" . dumb-jump-go-prefer-external-other-window)
              ("i" . dumb-jump-go-prompt)
              ("l" . dumb-jump-quick-look)
              ("b" . dumb-jump-back))
  :init
  (defvar my--dumb-jump-mode-map (make-sparse-keymap))
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-transient-map (dumb-jump-go
                      dumb-jump-go-other-window
                      dumb-jump-go-prefer-external
                      dumb-jump-go-prefer-external-other-window
                      dumb-jump-go-prompt
                      dumb-jump-quick-look)
    ("b" dumb-jump-back)))

(use-package eglot)

(use-package elfeed
  :bind (:map elfeed-show-mode-map
              ("e" . eww))
  :custom ((elfeed-feeds
            '(("https://reddit.com/r/linux/.rss" linux)
              ("https://reddit.com/r/programming/.rss" programming)
              ;; ("https://reddit.com/r/clojure/.rss" clojure)
              ;; ("https://clojure.org/feed.xml" clojure)
              ;; ("https://reddit.com/r/haskell/.rss" haskell)
              ("https://reddit.com/r/vim/.rss" vim)
              ("https://reddit.com/r/neovim/.rss" neovim)
              ("https://reddit.com/r/emacs/.rss" emacs)
              ("https://sachachua.com/blog/feed" emacs)
              ("https://pragmaticemacs.wordpress.com/feed/" emacs)
              ("https://hnrss.org/show?points=100&comments=25" programming)))
           (elfeed-search-filter "@2-days-ago +unread"))
  :config
  ;; WIP
  ;;   (progn
  ;;     (defun test-function (&optional args)
  ;;       (interactive
  ;;        (list (transient-args 'test-transient)))
  ;;       (message "args: %s" args))

  ;;     (cl-macrolet ((test-macrolet ()
  ;;                                  `(transient-define-prefix test-transient ()
  ;;                                     "Test Transient Title"
  ;;                                     ["Arguments"
  ;;                                      ("-d" "Days ago" "--days="
  ;;                                       :init-value (lambda (obj) (oset obj value "2")))
  ;;                                      ("-u" "Unread" "--unread"
  ;;                                       :init-value (lambda (obj) (oset obj value "--unread"))  "--unread")]
  ;;                                     ["Actions"
  ;;                                      ,@(cl-loop for i = 0 then (cl-incf i)
  ;;                                                 for f in (delete-dups (mapcar #'cadr elfeed-feeds))
  ;;                                                 collect (list (format "%d" i)
  ;;                                                               (format "%s" f)
  ;;                                                               #'test-function))])))
  ;;       (test-macrolet)
  ;;       (test-transient)))
  )

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package flymake
  :ensure nil
  :hook (flymake-mode . my--flymake-mode-hook-function)
  :bind (:map flymake-mode-map
              ("C-c f n" . flymake-goto-next-error)
              ("C-c f p" . flymake-goto-prev-error)
              ("C-c f d" . flymake-show-buffer-diagnostics))
  :config
  (defun my--flymake-mode-hook-function ()
    (setq next-error-function
          (lambda (arg reset)
            (unless reset
              (cond
               ((> arg 0) (flymake-goto-next-error arg))
               ((< arg 0) (flymake-goto-prev-error (abs arg))))))))
  (define-repeat-map (flymake-goto-next-error flymake-goto-prev-error)
    ("n" flymake-goto-next-error)
    ("p" flymake-goto-prev-error)))

(use-package gcmh
  :defer 1
  :diminish 'gcmh-mode
  :config (gcmh-mode 1))

(use-package grep
  :ensure nil
  :bind ("C-c g" . my-grep)
  :config
  (when (executable-find "rg")
    (grep-apply-setting 'grep-command "rg --color=auto --no-heading -nH --null -e ")
    (grep-apply-setting 'grep-template "rg <C> --no-heading -nH --null -e <R> <F>")
    (grep-apply-setting 'grep-find-command '("rg --color=auto --no-heading -nH --null -e " . 44))
    (grep-apply-setting 'grep-find-template "rg <C> --no-heading -nH --null -e <R> <D>")))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(use-package magit)

(use-package org
  :ensure nil
  :custom ((org-agenda-files '("~/Documents/org/agenda"))
           (holiday-bahai-holidays nil)
           (holiday-hebrew-holidays nil)
           (holiday-islamic-holidays nil))
  :bind ("C-c o a" . org-agenda)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t))))

(use-package org-modern
  :defer 1
  :init (global-org-modern-mode))

(use-package paredit)

(use-package popper
  :defer 1
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  (setq popper-reference-buffers '("\\*Messages\\*"
                                   "Output\\*$"
                                   "\\*Async Shell Command\\*"
                                   help-mode
                                   compilation-mode)
        popper-group-function #'popper-group-by-project)
  (define-repeat-map popper-cycle
    ("`" popper-cycle)))

(use-package prescient)

(use-package project
  :ensure nil
  ;; :bind (("M-n" . my-next-buffer)
  ;;        ("M-p" . my-previous-buffer))
  :config
  (advice-add 'my-next-buffer :after
              (lambda (&optional arg)
                (set-transient-map
                 (let ((map (make-sparse-keymap)))
                   (define-key map (kbd "n") (lambda () (interactive) (my-next-buffer arg)))
                   (define-key map (kbd "p") (lambda () (interactive) (my-previous-buffer arg)))
                   map))))

  (advice-add 'my-previous-buffer :after
              (lambda (&optional arg)
                (set-transient-map
                 (let ((map (make-sparse-keymap)))
                   (define-key map (kbd "n") (lambda () (interactive) (my-next-buffer arg)))
                   (define-key map (kbd "p") (lambda () (interactive) (my-previous-buffer arg)))
                   map))))

  (defun my-project-try-dotproject (dir)
    (when-let ((dir (locate-dominating-file dir ".project")))
      (cons 'dotproject dir)))

  (add-hook 'project-find-functions #'my-project-try-dotproject)

  (cl-defmethod project-root ((project (head dotproject)))
    (cdr project))

  (cl-defmethod project-files ((project (head dotproject)) &optional dirs)
    (cl-labels ((expand-dir (dir) (file-name-unquote (file-local-name (expand-file-name dir)))))
      (let* ((dirs (mapcar #'expand-dir
                           (or dirs
                               (list (project-root project)))))
             (dirs-string (string-join dirs " "))
             (command (if (executable-find "rg")
                          (format "rg --color=never -0 -l '' %s" dirs-string)
                        (format "grep --color=never -Z -l -I -r '' %s" dirs-string))))
        (with-temp-buffer
          (let ((status (process-file-shell-command command nil t)))
            (unless (zerop status)
              (error "File listing failed: %s" (buffer-string)))
            (split-string (buffer-string) "\0")))))))

(use-package pulsar
  :defer 1
  :init (pulsar-global-mode 1))

(use-package rainbow-mode)

(use-package recentf
  :ensure nil
  :init (recentf-mode 1))

(use-package repeat
  :if (fboundp 'repeat-mode)
  :ensure nil
  :init (repeat-mode 1))

(use-package rg
  :bind ("C-c r" . rg-menu))

(use-package savehist
  :ensure nil
  :defer 1
  :init (savehist-mode))

(use-package smartscan
  :hook (prog-mode . smartscan-mode))

(use-package so-long
  :ensure nil
  :defer 1
  :init (global-so-long-mode 1))

(use-package super-save
  :defer 1
  :diminish 'super-save-mode
  :init (super-save-mode +1)
  :config (setq auto-save-default nil))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :config
  (push (cons "." (expand-file-name "~/.emacs.d/undo-tree")) undo-tree-history-directory-alist))

(use-package visual-regexp
  :bind (("M-%" . vr/query-replace)
         ("C-M-%" . vr/replace)))

(use-package wgrep
  :config
  (defun my-wgrep-cheatsheet ()
    (interactive)
    (with-help-window (help-buffer)
      (princ "wgrep:

You can edit the text in the grep buffer after typing C-c C-p . After that the changed text is highlighted. The following keybindings are defined:
  - C-c C-e: Apply the changes to file buffers.
  - C-c C-u: All changes are unmarked and ignored.
  - C-c C-d: Mark as delete to current line (including newline).
  - C-c C-r: Remove the changes in the region (these changes are not applied to the files. Of course, the remaining changes can still be applied to the files.)
  - C-c C-p: Toggle read-only area.
  - C-c C-k: Discard all changes and exit.
  - C-x C-q: Exit wgrep mode.
  - To save all buffers that wgrep has changed, run
      M-x wgrep-save-all-buffers"))))

(use-package which-key
  :defer 1
  :diminish 'which-key-mode
  :init (which-key-mode))

(use-package whitespace-mode
  :ensure nil
  :hook (term-mode . (lambda () (interactive) (setq show-trailing-whitespace nil)))
  :init
  (setq whitespace-style '(face trailing tabs tab-mark))
  (global-whitespace-mode))

(use-package xref
  :ensure nil
  :config
  (when (executable-find "rg")
    (setq xref-search-program #'ripgrep)))

(require 'my-init-langs)
(require 'my-init-fun)
(require 'my-init-theme)
