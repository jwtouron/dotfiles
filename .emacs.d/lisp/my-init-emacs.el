;; -*- lexical-binding: t; -*-

;;;; Initialization for built-in stuff

(use-package better-defaults
  :demand t
  :init (defun helm-mode nil)) ; prevents better-defaults from using ido-mode

;; compilation

(custom-set-variables
 '(compilation-ask-about-save nil))
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; dired

(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "--group-directories-first -alhF")
           (dired-kill-when-opening-new-dired-buffer t)))

(use-package dired+
  :ensure nil
  :demand t
  :after dired
  :config (diredp-toggle-find-file-reuse-dir 1))

;; performance

(when (version<= "27.1" emacs-version)
  (setq bidi-inhibit-bpa t))
(setq-default bidi-paragraph-direction 'left-to-right)
(setq read-process-output-max (* 10 1000 1000))

;; whitespace

(setq whitespace-style '(face trailing tabs tab-mark))
(global-whitespace-mode)

;; misc

(custom-set-variables
 ;; '(global-auto-revert-non-file-buffers t)
 '(indicate-empty-lines t)
 '(inhibit-splash-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(show-trailing-whitespace t)
 '(tab-always-indent 'complete)
 '(kill-do-not-save-duplicates t)
 ;; Requires Emacs 28.1
 ;; '(xref-show-definitions-function #'xref-show-definitions-completing-read)
 )

(setq completion-cycle-threshold 3
      tab-always-indent 'complete)
(global-set-key (kbd "<C-mouse-4>") nil)
(global-set-key (kbd "<C-mouse-5>") nil)
(global-set-key (kbd "C-x 2") 'my-split-window-below)
(global-set-key (kbd "C-x 3") 'my-split-window-right)
(global-set-key (kbd "C-z") nil)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (defalias 'yes-or-no-p 'y-or-n-p))
;; (when (fboundp 'next-error-message-highlight)
;;   (customize-set-variable 'next-error-message-highlight t))
(column-number-mode t)
(global-auto-revert-mode)
;;(global-hl-line-mode)
(load custom-file t)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(advice-add 'shell
            :around
            (lambda (shell-fn &optional buffer)
              (let ((process-environment (nconc (cl-copy-list process-environment) (list "PAGER="))))
                (funcall shell-fn buffer))))

;; Non-pressing initialization

(let ((fn (lambda ()
            (global-so-long-mode 1)
            (recentf-mode 1)
            (savehist-mode 1)
            (when (fboundp 'repeat-mode)
              (repeat-mode 1)))))
  (add-hook 'after-init-hook
            (lambda ()
              (run-with-idle-timer 1 nil fn))))

(provide 'my-init-emacs)
