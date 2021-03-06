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
(defvar my-packages '(ac-cider
                      ace-jump-mode
                      auto-complete
                      better-defaults
                      bm
                      cider
                      cljsbuild-mode
                      clojure-mode
                      clojurescript-mode
                      color-theme-sanityinc-tomorrow
                      elixir-mode
                      flycheck
                      ghc
                      ghci-completion
                      go-mode
                      haskell-mode
                      ido-ubiquitous
                      key-chord
                      lua-mode
                      magit
                      markdown-mode
                      mediawiki
                      multiple-cursors
                      noctilux-theme
                      nzenburn-theme
                      paredit
                      paredit-everywhere
                      rainbow-delimiters
                      rust-mode
                      scala-mode2
                      slamhound
                      smart-tab
                      smex
                      solarized-theme
                      yaml-mode
                      zenburn-theme))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Non-package-managed elisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun my-string-suffix-p (str1 str2 &optional ignore-case)
  "Implementation of string-suffix-p for versions of Emacs prior to 24.4"
  (let ((begin2 (- (length str2) (length str1)))
        (end2 (length str2)))
    (when (< begin2 0) (setq begin2 0))
    (eq t (compare-strings str1 nil nil
                           str2 begin2 end2
                           ignore-case))))

;; compile
(setq compilation-ask-about-save nil)
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output t)
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c c") 'compile)))

;; macros
(defun my-macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
    With prefix argument, allows you to select what prompt string to use.
    If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
         (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                  (read-from-minibuffer prompt))))
    (unless (string= "" input) (insert input))))
(global-set-key (kbd "C-c q") 'my-macro-query)

;; join-line
(global-set-key (kbd "C-c j") 'join-line)

;; highlight every line
(global-hl-line-mode 1)

;; turn off blinking cursor
(blink-cursor-mode 0)

;; tags
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c t") 'find-tag)))

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; save every n keystrokes
(setq auto-save-interval 20)

;; paredit-everywhere
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;; electric-pair
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; whitespace
(setq-default show-trailing-whitespace t)
(setq whitespace-style '(tabs tab-mark))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'text-mode-hook 'whitespace-mode)

;; smarter indentation
(defun my-smart-newline-and-indent ()
  (interactive)
  (let* ((tokens '(("(" . ")") ("{" . "}") ("[" . "]")))
         (before-point (buffer-substring-no-properties (line-beginning-position) (point)))
         (after-point (buffer-substring-no-properties (point) (line-end-position)))
         (point-between-tokens (reduce (lambda (b tokens)
                                         (or b (and (my-string-suffix-p (car tokens) before-point)
                                                    (string-prefix-p (cdr tokens) after-point))))
                                       tokens :initial-value nil)))
    (if point-between-tokens
        (progn (newline-and-indent)
               (newline-and-indent)
               (previous-line)
               (indent-according-to-mode))
      (newline-and-indent))))
(global-set-key (kbd "RET") 'my-smart-newline-and-indent)
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

;; hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers))

;; replace yes-or-no-p with y-or-n-p
(defalias 'yes-or-no-p 'y-or-n-p)

;; rainbow delimiters
(global-rainbow-delimiters-mode)

;; General Auto-Complete
(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-indication-mode nil)

;; ido
(setq ido-use-virtual-buffers t)
(ido-ubiquitous-mode t)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Windows
(when (string-equal system-type "windows-nt")
  (when (not (getenv "COMSPEC"))
    (setenv "COMSPEC" "cmd.exe"))
  (setq tramp-encoding-shell "cmd\\.exe"))

;; don't show the emacs start screen
(setq inhibit-startup-message t)

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
(ignore-errors (set-frame-font (format "%s-10" "DejaVu Sans Mono")))

;;; Language specific settings

;; c-mode
(setq c-default-style "stroustrup")

;; cc-mode
(add-hook 'c++-mode-hook
          (lambda ()
            (c-add-style "my-cc-style"
                         '("stroustrup"
                           (c-offsets-alist . ((innamespace . [0])))))
            (c-set-style "my-cc-style")))

;; clojure-mode
(add-hook 'clojure-mode-hook
          (lambda ()
            (progn
              (mapc (lambda (x) (put-clojure-indent x 'defun))
                    '(lazy-seq cond dosync))
              (paredit-mode))))

(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u03BB") nil))))))

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; ac-cider (Auto-complete for Cider)
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

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
            (turn-on-haskell-indentation)
            (inf-haskell-mode)))

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
(load-theme 'zenburn t)
