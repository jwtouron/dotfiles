;; -*- lexical-binding: t; -*-

(use-package better-defaults
  :demand t
  :config
  ;; (delete-selection-mode t)

  ;; aliases
  (defalias 'ac 'aya-create)
  (defalias 'ae 'aya-expand)
  (defalias 'ar 'align-regexp)
  (defalias 'nd 'narrow-or-widen-dwim)
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq initial-major-mode 'fundamental-mode
        find-file-visit-truename t)
  (put 'narrow-to-region 'disabled nil)
  (global-auto-revert-mode t)
  (winner-mode 1)
  (recentf-mode 1)
  (add-hook 'prog-mode-hook 'electric-pair-local-mode)

  ;; saving
  (add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

  ;; Prevent customizations from being written in init.el
  (setq custom-file "~/.emacs-custom.el")
  ;;(load custom-file t t)

  ;; Compile
  (setq-default compilation-ask-about-save nil
                compilation-auto-jump-to-first-error t
                compilation-scroll-output t)

  ;; hippie-expand
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name))

  ;; ibuffer
  (setq ibuffer-saved-filter-groups
        '(("Default"
           ("Clojure" (or (mode . clojure-mode)
                          (mode . clojurescript-mode)
                          (mode . clojurec-mode)
                          (name . "^\\*cider")))
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")))
           ("Emacs Lisp" (mode . emacs-lisp-mode))
           ("Haskell" (mode . haskell-mode))
           ("Python" (mode . python-mode)))))
  (defun my-ibuffer-mode-hook ()
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "Default"))
  (add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)

  ;; performance
  (when (boundp 'w32-pipe-buffer-size)
    (setq w32-pipe-buffer-size (* 10 1024 1024)))
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  (when (boundp 'w32-get-true-file-attributes)
    (setq w32-get-true-file-attributes nil))
  (setq fast-but-imprecise-scrolling t
        ffap-machine-p-known 'reject
        frame-inhibit-implied-resize t
        highlight-nonselected-windows nil
        inhibit-compacting-font-caches t ; supposedly the default with Emacs 27.1
        jit-lock-stealth-time 5
        jit-lock-defer-time 0
        read-process-output-max (* 10 1024 1024)
        x-wait-for-event-timeout nil) ; supposedly unnecessary with Emacs 27
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right
                cursor-in-non-selected-windows nil)
  ;; gcmh
  ;; (add-hook 'after-init-hook
  ;;           (lambda ()
  ;;             (setq gc-cons-percentage 0.5)
  ;;             (if (boundp 'after-focus-change-function)
  ;;                 (add-function :after after-focus-change-function
  ;;                               (lambda ()
  ;;                                 (unless (frame-focus-state)
  ;;                                   (garbage-collect))))
  ;;               (add-hook 'focus-out-hook 'garbage-collect))
  ;;             (run-with-idle-timer 5 t 'garbage-collect)))

  ;; coding systems
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (if (boundp 'buffer-file-coding-system)
      (setq-default buffer-file-coding-system 'utf-8)
    (setq default-buffer-file-coding-system 'utf-8))
  (set-language-environment 'utf-8)

  ;; visual stuff

  (setq inhibit-startup-message t
        column-number-mode t
        frame-resize-pixelwise t)
  (setq-default frame-title-format '("%f [%m]")
                indicate-empty-lines t)
  (global-hl-line-mode 1)
  (my-set-font)

  ;; whitespace
  (setq-default show-trailing-whitespace t
                whitespace-style '(tabs tab-mark))
  (global-whitespace-mode 1)

  ;; Windows
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic)))

  )

(provide 'my-init-better-defaults)
