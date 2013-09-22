;; package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

;; install desired packages
(defvar my-packages '(ace-jump-mode
                      bm
                      clj-refactor
                      cljsbuild-mode
                      clojure-mode
                      clojurescript-mode
                      flymake
                      flymake-cursor
                      flymake-easy
                      flymake-haskell-multi
                      flymake-hlint
                      ghc
                      ghci-completion
                      haskell-mode
                      hippie-expand-haskell
                      key-chord
                      lua-mode
                      magit
                      markdown-mode
                      mediawiki
                      multiple-cursors
                      nrepl
                      nzenburn-theme
                      paredit
                      slamhound
                      smart-tab
                      solarized-theme
                      starter-kit
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-js
                      starter-kit-lisp
                      starter-kit-ruby
                      yaml-mode
                      zenburn-theme))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; required for several packages
(require 'cl)

;; Non-package-managed elisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; thrift-mode
(require 'thrift-mode)

;; yaml mode
(require 'yaml-mode)

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

;; clojure-mode
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook (lambda () (define-clojure-indent
                                          (lazy-seq 'defun)
                                          (cond 'defun))))

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; nrepl-mode
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(setq nrepl-popup-stacktraces nil)

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

;; change zap-to-char to leave the character being zapped to
(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                       (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; dired-mode - enter doesn't open a new buffer
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "RET") #'dired-find-alternate-file)))

;; automatically load files as they change on disk
(global-auto-revert-mode t)

;; always show row/column numbers
(setq column-number-mode t)

;; c-mode
(setq c-default-style "stroustrup")

;; bm (bookmarks)
(global-set-key (kbd "C-.") 'bm-toggle)
(global-set-key (kbd "C-,") 'bm-next)
(setq bm-highlight-style 'bm-highlight-only-fringe)
(setq bm-cycle-all-buffers t)

;; smart-tab-mode
(setq smart-tab-using-hippie-expand t)
(add-to-list 'smart-tab-disabled-major-modes 'haskell-mode)

;; scrolling moves half a window at a time
(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))
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

;; use DejaVu Sans Mono if available
(defvar my-desired-fonts '("DejaVu Sans Mono"))
(let ((font (loop for font in my-desired-fonts
                  when (member font (font-family-list))
                  return font)))
  (when font
    (set-frame-font (format "%s-10" font))))

;; change fn to lambda in clojure-mode
(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u03BB") nil))))))

;; js-mode
(add-hook 'js-mode-hook
          (lambda ()
            (font-lock-add-keywords
             'js-mode `(("\\(function *\\)("
                         (0 (progn (compose-region (match-beginning 1)
                                                   (match-end 1) "\u03BB")
                                   nil)))))))
(add-hook 'js-mode-hook (lambda () (setq js-indent-level 4)))

;; python-mode
(setq python-remove-cwd-from-path nil)

;; haskell-mode
(add-hook 'haskell-mode-hook
          (lambda ()
            (setq haskell-font-lock-symbols 'unicode)
            (setq haskell-program-name "ghci")
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indent)
            (define-key haskell-mode-map (kbd "C-c >") 'haskell-move-nested-right)
            (define-key haskell-mode-map (kbd "C-c <") 'haskell-move-nested-left)))

;; lua-mode
(setq lua-indent-level 4)

;; scala-mode
(defun my-scala-load-file (ag)
  (interactive "P")
  (unless (scala-interpreter-running-p-1)
    (scala-run-scala scala-interpreter)
    (previous-multiframe-window))
  (save-buffer)
  (let ((file-name (if current-prefix-arg
                       (car (comint-get-source "Load Scala file: " scala-prev-l/c-dir/file
                                               '(scala-mode) t))
                     buffer-file-name)))
    (when current-prefix-arg
      (comint-check-source file-name))
    (setq scala-prev-l/c-dir/file (cons (file-name-directory file-name)
                                        (file-name-nondirectory file-name)))
    (scala-send-string ":load %s" file-name)))
(add-hook 'scala-mode-hook (lambda () (define-key scala-mode-map (kbd "C-c C-l") 'my-scala-load-file)))

(defun my-scala-eval-buffer ()
  (interactive)
  (unless (scala-interpreter-running-p-1)
    (scala-run-scala scala-interpreter)
    (previous-multiframe-window))
  (let* ((s (buffer-string))
         (s (replace-regexp-in-string "^[[:space:]]*\n" "" s))
         (s (replace-regexp-in-string "^/.*\n" "" s)))
    (comint-send-string scala-inf-buffer-name s)
    (comint-send-string scala-inf-buffer-name "\n")))
(add-hook 'scala-mode-hook (lambda () (define-key scala-mode-map (kbd "C-c C-b") 'my-scala-eval-buffer)))

(defun my-scala-restart ()
  (interactive)
  (scala-quit-interpreter)
  (sleep-for .1)
  (scala-run-scala scala-interpreter)
  (previous-multiframe-window))

;; load desired theme
(load-theme 'nzenburn t)
