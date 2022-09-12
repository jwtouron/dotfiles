;; -*- lexical-binding: t; -*-

;;; bootstrapping

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

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
(require 'my-emacs-init)
;; (require 'my-keybindings)
(require 'my-init-cemov)

(use-package ace-window
  :bind ("C-x o" . 'ace-window))

(use-package avy
  :bind ("C-c j" . 'avy-goto-char-timer))

(use-package calfw
  :commands (cfw:open-calendar-buffer))

(use-package calfw-org
  :commands (cfw:open-org-calendar cfw:org-create-source)
  :init
  (defun my-open-calendar ()
    (interactive)
    (my-org-agenda-files-refresh)
    (cfw:open-calendar-buffer
     :contents-sources (list (cfw:org-create-source)))))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
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
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package citre)

;; (use-package company
;;   :hook (after-init . global-company-mode)
;;   :bind (:map company-mode-map
;;               ("M-/" . company-complete-common))
;;   :custom ((company-minimum-prefix-length 1)
;;            (company-idle-delay nil)))

(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))

(use-package corfu
  :hook (minibuffer-setup . corfu-enable-in-minibuffer)
  :init
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (global-corfu-mode))

(use-package corfu-doc
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
              ("M-p" . corfu-doc-scroll-down)
              ("M-n" . corfu-doc-scroll-up)
              ("M-d" . corfu-doc-toggle)))

;; (use-package counsel
;;   :bind (("C-c c b" . counsel-bookmark)
;;          ("C-c c c" . counsel-compile)
;;          ("C-c c d" . counsel-descbinds)
;;          ("C-c c F" . counsel-org-file)
;;          ("C-c c g" . counsel-git)
;;          ("C-c c J" . counsel-file-jump)
;;          ("C-c c j" . counsel-git-grep)
;;          ("C-c c k" . counsel-rg)
;;          ("C-c c l" . counsel-locate)
;;          ("C-c c L" . counsel-git-log)
;;          ("C-c c m" . counsel-linux-app)
;;          ("C-c c n" . counsel-fzf)
;;          ("C-c c o" . counsel-outline)
;;          ("C-c c r" . ivy-resume)
;;          ("C-c c t" . counsel-load-theme)
;;          ("C-c c w" . counsel-wmctrl))
;;   :init
;;   (setq counsel-describe-function-function #'helpful-callable
;;         counsel-describe-variable-function #'helpful-variable)
;;   (counsel-mode))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-x x r" . crux-rename-file-and-buffer)))

(use-package dabbrev
  :ensure nil
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

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
              ("l" . dumb-jump-quick-look))
  :hook (prog-mode . dumb-jump-mode)
  :init
  (defvar my--dumb-jump-mode-map (make-sparse-keymap))
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (when (and (boundp 'xref-show-definitions-function)
             (fboundp 'xref-show-definitions-completing-read))
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  :config
  ;; (when (fboundp 'ivy-mode)
  ;;   (custom-set-variables '(dumb-jump-selector 'ivy)))
  (dolist (func '(dumb-jump-go
                  dumb-jump-go-other-window
                  dumb-jump-go-prefer-external
                  dumb-jump-go-prefer-external-other-window
                  dumb-jump-go-prompt
                  dumb-jump-quick-look))
    (advice-add-repeat-mode func
                            '("b" . dumb-jump-back))))

(use-package eglot)

(use-package elfeed
  :bind (:map elfeed-show-mode-map
              ("e" . eww))
  :custom ((elfeed-feeds
            '(("https://reddit.com/r/linux/.rss" linux)
              ("https://reddit.com/r/programming/.rss" programming)
              ("https://reddit.com/r/clojure/.rss" clojure)
              ("https://clojure.org/feed.xml" clojure)
              ("https://reddit.com/r/haskell/.rss" haskell)
              ("https://reddit.com/r/vim/.rss" vim)
              ("https://reddit.com/r/emacs/.rss" emacs)
              ("https://hnrss.org/show?points=100&comments=25" hacker-news)))
           (elfeed-search-filter "@2-days-ago +unread")))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package flymake
  :ensure nil
  :hook ((flymake-mode . my--init-flymake-mode))
  :config
  (defun my--init-flymake-mode ()
    (local-set-key (kbd "C-c f n") 'flymake-goto-next-error)
    (local-set-key (kbd "C-c f p") 'flymake-goto-prev-error)
    (local-set-key (kbd "C-c f d") 'flymake-show-buffer-diagnostics))
  (advice-add-repeat-mode 'flymake-goto-next-error
                          '("n" . flymake-goto-next-error)
                          '("p" . flymake-goto-prev-error))
  (advice-add-repeat-mode 'flymake-goto-prev-error
                          '("n" . flymake-goto-next-error)
                          '("p" . flymake-goto-prev-error)))

(use-package gcmh
  :defer 1
  :diminish 'gcmh-mode
  :config (gcmh-mode 1))

(use-package grep
  :ensure nil
  :bind ("C-c g" . my-grep)
  :config
  (when (executable-find "_rg")
    (grep-apply-setting 'grep-command "rg --color=auto -nH --null -e ")
    (grep-apply-setting 'grep-template "rg --null -nH --no-heading --no-messages -g '!*/' -e <R>")
    (grep-apply-setting 'grep-find-command '("rg --color=auto -nH --null -e " . 31))
    (grep-apply-setting 'grep-find-template "rg <C> -nH --null -e <R> <D>")))

(use-package helpful
  :bind (("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))

;; (use-package ivy
;;   :init (ivy-mode)
;;   :config
;;   (setq ivy-use-virtual-buffers t
;;         enable-recursive-minibuffers t)
;;   (defun my-ivy-help ()
;;     (interactive)
;;     (with-help-window (help-buffer)
;;       (princ "C-c C-o ivy-occur"))))

;; (use-package ivy-prescient
;;   :after ivy
;;   :init (ivy-prescient-mode))

;; (use-package ivy-xref
;;   :init
;;   (when (>= emacs-major-version 27)
;;     (setq xref-show-definitions-function #'ivy-xref-show-defs))
;;   (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; (use-package lsp-mode
;;   :custom ((lsp-enable-snippet nil)
;;            (lsp-enable-symbol-highlighting nil)
;;            (lsp-headerline-breadcrumb-enable nil)
;;            (lsp-keymap-prefix "C-c l")
;;            (lsp-lens-enable nil))
;;   :hook (lsp-mode . lsp-enable-which-key-integration))

(use-package magit)

(use-package multiple-cursors
  :custom ((mc/always-run-for-all t))
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this-dwim)))

(use-package org
  :ensure nil
  :bind ("C-c o a" . org-agenda)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t))))

(use-package org-modern
  :defer 1
  :init (global-org-modern-mode))

(use-package org-roam
  :bind ("C-c o r" . org-roam-node-find)
  :commands (org-roam-node-list)
  :custom (org-roam-directory "~/Documents/org/roam")
  :init
  (defun my-org-agenda-files-refresh ()
    (interactive)
    (setq org-agenda-files
          (mapcar #'org-roam-node-file
                  (seq-filter (lambda (n)
                                (member "agenda" (org-roam-node-tags n)))
                              (org-roam-node-list)))))
  (advice-add 'org-agenda :before (lambda (&rest _) (my-org-agenda-files-refresh)))
  :config (org-roam-db-autosync-enable))

(use-package paredit)

(use-package popper
  :defer 1
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq  popper-reference-buffers '("\\*Messages\\*"
                                    "Output\\*$"
                                    "\\*Async Shell Command\\*"
                                    help-mode
                                    compilation-mode)
         popper-window-height 0.33
         popper-group-function #'popper-group-by-project)
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package prescient)

(use-package project
  :ensure nil
  :bind (("M-n" . my-next-buffer)
         ("M-p" . my-previous-buffer))
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

;; (use-package projectile
;;   :commands (projectile-project-root)
;;   :custom ((projectile-indexing-method 'alien))
;;   :bind
;;   ((:map projectile-mode-map
;;          ("C-c p" . 'projectile-command-map)))
;;   :init (projectile-mode +1))

(use-package pulsar
  :defer 1
  :init (pulsar-global-mode 1))

(use-package rainbow-mode)

(use-package rg)

(use-package savehist
  :ensure nil
  :defer 1
  :init (savehist-mode))

(use-package smartscan
  :if nil
  :defer 1
  :hook (prog-mode . smartscan-mode))

(use-package super-save
  :defer 1
  :diminish 'super-save-mode
  :init (super-save-mode +1)
  :config (setq auto-save-default nil))

(use-package undo-tree
  :defer 1
  :custom (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  :init (global-undo-tree-mode))

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

(use-package xref
  :ensure nil
  :config
  (when (executable-find "rg")
    (setq xref-search-program #'ripgrep)))

(require 'my-init-langs)
(require 'my-init-fun)
(require 'my-init-theme)
