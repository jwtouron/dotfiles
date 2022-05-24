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
(require 'my-theme-init)

(use-package ace-window
  :bind ("C-x o" . 'ace-window))

(use-package all-the-icons
  :if (display-graphic-p))
;; Run `all-the-icons-install-fonts' to install
;; On windows: this will only download the fonts--they must be installed manually

(use-package avy
  :bind ("C-;" . 'avy-goto-char-timer))

(use-package calfw
  :commands (cfw:open-calendar-buffer))

(use-package calfw-org
  :commands (cfw:open-org-calendar cfw:org-create-source)
  :init
  (defun my-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources (list (cfw:org-create-source)))))

(use-package citre)

(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-mode-map
              ("M-/" . company-complete-common))
  :custom ((company-minimum-prefix-length 1)
           (company-idle-delay nil)))

(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))

(use-package counsel
  :init (counsel-mode)
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-x x r" . crux-rename-file-and-buffer)))

(use-package dabbrev
  :if nil
  :ensure nil
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(use-package diminish)

(use-package dot-mode
  :defer 1
  :diminish 'dot-mode
  :init (global-dot-mode t))

(use-package dumb-jump
  :bind (("C-M-j" . dumb-jump-hydra/body))
  :hook (prog-mode . dumb-jump-mode)
  :custom (dumb-jump-selector 'ivy)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (when (and (boundp 'xref-show-definitions-function)
             (fboundp 'xref-show-definitions-completing-read))
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  :config
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")
    ("q" nil "Quit")))

(use-package easy-kill
  :init (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package elfeed
  :bind (:map elfeed-show-mode-map
              ("e" . eww))
  :custom (elfeed-feeds
           '(("https://reddit.com/r/linux/.rss" linux)
             ("https://reddit.com/r/programming/.rss" programming)
             ("https://reddit.com/r/clojure/.rss" clojure)
             ("https://reddit.com/r/haskell/.rss" haskell)
             ("https://reddit.com/r/vim/.rss" vim)
             ("https://reddit.com/r/emacs/.rss" emacs)))
  :config (elfeed-goodies/setup))

(use-package elfeed-goodies
  :custom ((elfeed-goodies/entry-pane-position 'bottom)
           (elfeed-goodies/powerline-default-separator nil)))

(use-package expand-region
  :bind ("C-=" . #'my-expand-region)
  :init
  (defun my-expand-region ()
    (interactive)
    (er/expand-region 1)
    (hydra-expand-region/body))
  :config
  (defhydra hydra-expand-region ()
    "expand-region"
    ("=" er/expand-region "er/expand-region")
    ("C-=" er/expand-region "er/expand-region")
    ("-" er/contract-region "er/contract-region")
    ("C--" er/contract-region "er/contract-region")))

(use-package flycheck
  :custom (flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package flycheck-pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  :custom (flycheck-pos-tip-timeout 0))

(use-package gcmh
  :defer 1
  :diminish 'gcmh-mode
  :config (gcmh-mode 1))

(use-package hydra
  :bind (("M-g n" . my-next-error)
         ("M-g M-n" . my-next-error)
         ("M-g p" . my-previous-error)
         ("M-g M-p" . my-previous-error)
         ("C-x `" . my-next-error)

         ("C-x ^" . hydra-size-window/body))
  :init
  (defun my-next-error ()
    (interactive)
    (next-error)
    (hydra-navigate-errors/body))
  (defun my-previous-error ()
    (interactive)
    (previous-error)
    (hydra-navigate-errors/body))
  :config
  (defhydra hydra-navigate-errors ()
    "navigate-errors"
    ("n" next-error "next-error")
    ("M-n" next-error "next-error")
    ("p" previous-error "previous-error")
    ("M-p" previous-error "previous-error")
    ("`" next-error "next-error")
    ("S-`" previous-error "previous-error"))
  (defhydra hydra-size-window ()
    "Change window size"
    ("+" enlarge-window "enlarge-window")
    ("=" enlarge-window "enlarge-window")
    ("-" shrink-window "shrink-window")
    ("q" nil "quit")))

(use-package ivy
  :diminish 'ivy-mode
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (ivy-mode)
  :config
  (defun my-ivy-help ()
    (interactive)
    (with-help-window (help-buffer)
      (princ "ivy:

C-c C-o ivy-occur"))))

(use-package ivy-prescient
  :init (ivy-prescient-mode))

(use-package helpful
  :bind (("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(use-package lsp-mode
  :custom ((lsp-enable-snippet nil)
           (lsp-enable-symbol-highlighting nil)
           (lsp-headerline-breadcrumb-enable nil)
           (lsp-keymap-prefix "C-c l")
           (lsp-lens-enable nil))
  :hook (lsp-mode . lsp-enable-which-key-integration))

(use-package magit)

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . hydra-multiple-cursors/body)
         ("C-c m" . hydra-multiple-cursors/body))
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("|" mc/vertical-align)
    ("s" mc/mark-all-in-region-regexp :exit t)
    ("0" mc/insert-numbers :exit t)
    ("A" mc/insert-letters :exit t)
    ("<mouse-1>" mc/add-cursor-on-click)
    ;; Help with click recognition in this hydra
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q" nil)))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package paredit)

(use-package prescient)

(use-package projectile
  :commands (projectile-project-root)
  :custom ((projectile-indexing-method 'alien))
  :bind
  ((:map projectile-mode-map
         ("C-c p" . 'projectile-command-map)))
  :init (projectile-mode +1))

(use-package quickrun)

(use-package rainbow-mode)

(use-package rg)

(use-package rust-mode
  :hook (rust-mode . my--init-rust-mode)
  :bind (:map rust-mode-map
         ("C-c m r" . 'rust-run)
         ("C-c m c" . 'rust-compile)
         ("C-c m t" . 'rust-test)
         ("C-c m k" . 'rust-check))
  :config
  (defun my--init-rust-mode ()
    (electric-pair-local-mode)
    (flycheck-mode)))

(use-package smartscan
  :defer 1
  :config (global-smartscan-mode 1))

(use-package super-save
  :defer 1
  :diminish 'super-save-mode
  :init (super-save-mode +1)
  :config (setq auto-save-default nil))

(use-package visual-regexp
  :bind (("M-%" . vr/query-replace)
         ("C-M-%" . vr/replace)))

(use-package wgrep
  :config
  (defun my-wgrep-help ()
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

(require 'my-language-init)
