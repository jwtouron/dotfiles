;; -*- lexical-binding: t; -*-

(let ((keymap (make-sparse-keymap))
      (bindings
       '(
         ;; misc
         (";" . execute-extended-command)
         ("C-;" . execute-extended-command)

         ;; buffer
         ("bb" . ivy-switch-buffer)
         ("bB" . (lambda ()
                   (interactive)
                   (select-window (split-window-below))
                   (ivy-switch-buffer)))
         ("bk" . kill-this-buffer)

         ;; flycheck
         ;; ("cc" . flycheck-buffer)
         ;; ("cf" . flycheck-first-error)
         ;; ("cn" . next-error)
         ;; ("cp" . previous-error)
         ;; ("cn" . flycheck-next-error)
         ;; ("cp" . flycheck-previous-error)

         ;; errors
         ("en" . my-next-error)
         ("ep" . my-previous-error)

         ;; file
         ("fe" . crux-find-user-init-file)
         ("ff" . find-file)
         ("fo" . (lambda () (interactive) (find-file "~/Documents/org/index.org")))
         ("fs" . save-some-buffers)
         ("fS" . save-buffer)

         ;; help
         ("hf" . counsel-describe-function)
         ("hk" . describe-key)
         ("hv" . counsel-describe-variable)

         ("j" . avy-goto-char-timer)

         ;; ("p*" . ...) Used by projectile, see init.el

         ;; quit
         ("qq" . save-buffers-kill-terminal)

         ;; register
         ;; ("r SPC" . point-to-register)
         ;; ("rj" . jump-to-register)

         ;; search
         ;; ("sc" . counsel-rg)
         ;; ("sd" . deadgrep)
         ;; ("sd" . rg-dwim)
         ;; ("sk" . rg-kill-saved-searches)
         ;; ("sl" . rg-list-searches)
         ;; ("sp" . rg-project)
         ;; ("sr" . rg)
         ;; ("ss" . rg-save-search)
         ;; ("sS" . rg-save-search-as-name)
         ;; ("st" . rg-literal)

         ;; window
         ("wd" . delete-window)
         ("w0" . delete-window)
         ("wo" . delete-other-windows)
         ("w1" . delete-other-windows)
         ;; ("wr" . hydra-window/body)
         ("ws" . split-window-below)
         ("wv" . split-window-right)
         ("ww" . ace-window)
         )))

  (global-set-key (kbd "C-;") keymap)
  (dolist (binding bindings)
    (define-key keymap (kbd (car binding)) (cdr binding))))

(provide 'my-keybindings)
