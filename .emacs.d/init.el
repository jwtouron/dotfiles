;; required for several packages
(require 'cl)

;; package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

;; install desired packages
(defvar my-packages '(ace-jump-mode
                      better-defaults
                      bm
                      cider
                      clj-refactor
                      cljsbuild-mode
                      clojure-mode
                      clojurescript-mode
                      elixir-mode
                      flycheck
                      ghc
                      ghci-completion
                      go-mode
                      haskell-mode
                      hi2
                      hippie-expand-haskell
                      ido-ubiquitous
                      key-chord
                      lua-mode
                      magit
                      markdown-mode
                      mediawiki
                      multiple-cursors
                      nzenburn-theme
                      paredit
                      rust-mode
                      scala-mode2
                      slamhound
                      smart-tab
                      solarized-theme
                      yaml-mode
                      zenburn-theme))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Non-package-managed elisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(global-set-key (kbd "M-O") 'smart-open-line)

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (delete-horizontal-space)
  (move-beginning-of-line nil)
  (open-line 1)
  (indent-according-to-mode))
(global-set-key (kbd "M-o") 'smart-open-line-above)

;; highlight every line
(global-hl-line-mode 1)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; turn off auto-fill-mode - starter-kit adds these hooks
(remove-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; save every n keystrokes
(setq auto-save-interval 20)

;; remove line highlighting
(remove-hook 'prog-mode-hook 'esk-turn-on-hi-line-mode)

;; remove word highlighting
(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

;; incremental searches always put point at search string beginning on exit
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

;; whitespace
(setq-default show-trailing-whitespace t)
(setq whitespace-style '(tabs tab-mark))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'text-mode-hook 'whitespace-mode)

;; smarter indentation
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

;; dired-mode - enter doesn't open a new buffer
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "RET") #'dired-find-alternate-file)))

;; automatically load files as they change on disk
(global-auto-revert-mode t)

;; always show row/column numbers
(setq column-number-mode t)

;; bm (bookmarks)
(global-set-key (kbd "C-.") 'bm-toggle)
(global-set-key (kbd "C-,") 'bm-next)
(setq bm-highlight-style 'bm-highlight-only-fringe)
(setq bm-cycle-all-buffers t)
(setq bm-recenter t)
(setq bm-goto-position nil)

;; smart-tab-mode
(require 'smart-tab)
(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode 1)
(add-to-list 'smart-tab-disabled-major-modes 'haskell-mode)

;; replace yes-or-no-p with y-or-n-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ido
(ido-ubiquitous-mode t)

;; don't show the emacs start screen
(setq inhibit-startup-message t)

;; enable iswitchb
(iswitchb-mode 1)

;; indicate end of file
(setq default-indicate-empty-lines t)

;; scrolling moves half a window at a time
(defun window-half-height ()
  (/ (window-height (selected-window)) 2))
(global-set-key (kbd "C-v")
                (lambda ()
                  (interactive)
                  (next-line (window-half-height))
                  (recenter)))
(global-set-key (kbd "M-v")
                (lambda ()
                  (interactive)
                  (previous-line (window-half-height))
                  (recenter)))

;; switch-to-previous-buffer
(defun switch-to-previous-buffer ()
  "Switch to last buffer visited"
  (interactive)
  (switch-to-buffer (other-buffer)))

;; key chords
(require 'key-chord)
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)
(key-chord-define-global "kk" 'just-one-space)
(key-chord-define-global "KK" 'delete-horizontal-space)
(key-chord-define-global "JJ" 'switch-to-previous-buffer)
(key-chord-mode +1)

;; use DejaVu Sans Mono
(set-frame-font (format "%s-10" "DejaVu Sans Mono"))

;;;; Language specific settings

;; c-mode
(setq c-default-style "stroustrup")

;; clojure-mode
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook (lambda () (define-clojure-indent
                                     (lazy-seq 'defun)
                                     (cond 'defun))))

(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u03BB") nil))))))

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-popup-stacktraces t)

(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; haskell-mode
(add-hook 'haskell-mode-hook
          (lambda ()
            (setq haskell-font-lock-symbols 'unicode)
            (setq haskell-program-name "ghci")
            (turn-on-haskell-doc-mode)
            (define-key haskell-mode-map (kbd "C-c >") 'haskell-move-nested-right)
            (define-key haskell-mode-map (kbd "C-c <") 'haskell-move-nested-left)
            (turn-on-hi2)
            (set-up-haskell-hippie-expand)))

;; js-mode
(add-hook 'js-mode-hook
          (lambda ()
            (font-lock-add-keywords
             'js-mode `(("\\(function *\\)("
                         (0 (progn (compose-region (match-beginning 1)
                                                   (match-end 1) "\u03BB")
                                   nil)))))))

;; python-mode
(setq python-remove-cwd-from-path nil)

;; thrift-mode
(require 'thrift-mode)

;; yaml mode
(require 'yaml-mode)

;; load desired theme
(load-theme 'nzenburn t)
