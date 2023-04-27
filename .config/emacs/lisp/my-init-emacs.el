;; -*- lexical-binding: t; -*-

;;;; Initialization for built-in stuff

(use-package better-defaults
  :demand t
  :init (defun helm-mode nil)  ; prevents better-defaults from using ido-mode
  :config (load custom-file t))

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
  :custom ((dired-listing-switches "-alhF")
           (dired-kill-when-opening-new-dired-buffer t)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; performance

(when (version<= "27.1" emacs-version)
  (setq bidi-inhibit-bpa t))
(setq-default bidi-paragraph-direction 'left-to-right)
(setq read-process-output-max (* 10 1000 1000))

;; misc

(custom-set-variables
 ;; '(global-auto-revert-non-file-buffers t)
 '(indicate-empty-lines t)
 '(inhibit-splash-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(show-trailing-whitespace t)
 '(tab-always-indent 'complete)
 '(kill-do-not-save-duplicates t))

(setq completion-cycle-threshold 3)
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
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(advice-add 'shell
            :around
            (lambda (shell-fn &optional buffer)
              (let ((process-environment (nconc (cl-copy-list process-environment) (list "PAGER="))))
                (funcall shell-fn buffer))))

(provide 'my-init-emacs)
