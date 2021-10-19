;; -*- lexical-binding: t; -*-

(let ((keymap (make-sparse-keymap))
      (bindings
       '(
         ("C-;" . counsel-M-x)
         (";" . counsel-M-x)

         ("bb" . ivy-switch-buffer)
         ("bB" . ivy-switch-buffer-other-window)
         ("bk" . kill-this-buffer)

         ("cc" . flycheck-buffer)
         ("cf" . flycheck-first-error)
         ("cn" . next-error)
         ("cp" . previous-error)
         ;; ("cn" . flycheck-next-error)
         ;; ("cp" . flycheck-previous-error)

         ("d" . devdocs-search)

         ("fe" . crux-find-user-init-file)
         ("ff" . counsel-find-file)
         ("fo" . (lambda () (interactive) (find-file "~/Documents/org/index.org")))
         ("fs" . (lambda () (interactive) (save-some-buffers t)))
         ("fS" . save-some-buffers)

         ("hf" . counsel-describe-function)
         ("hk" . describe-key)
         ("hv" . counsel-describe-variable)

         ("ii" . counsel-imenu)
         ("ir" . counsel-rg)
         ("is" . swiper)
         ("iS" . swiper-all)

         ("j" . avy-goto-char)

         ;; ("p*" . ...) Used by projectile, see init.el

         ("qq" . save-buffers-kill-terminal)

         ("r SPC" . point-to-register)
         ("rj" . jump-to-register)

         ;; ("sc" . counsel-rg)
         ;; ("sd" . deadgrep)
         ("sd" . rg-dwim)
         ("sk" . rg-kill-saved-searches)
         ("sl" . rg-list-searches)
         ("sp" . rg-project)
         ("sr" . rg)
         ("ss" . rg-save-search)
         ("sS" . rg-save-search-as-name)
         ("st" . rg-literal)


         ("wd" . delete-window)
         ("wo" . delete-other-windows)
         ("wr" . hydra-window/body)
         ("ws" . split-window-below)
         ("wS" . (lambda () (interactive) (select-window (split-window-below))))
         ("wv" . split-window-right)
         ("ww" . ace-window)

         )))

  ;; (global-set-key (kbd "C-;") keymap)
  (global-set-key (kbd "C-c") keymap)
  (dolist (binding bindings)
    (define-key keymap (kbd (car binding)) (cdr binding))))

(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-c w k") 'daedreth/kill-inner-word)
(global-set-key (kbd "C-c w c") 'daedreth/copy-whole-word)
(global-set-key (kbd "C-c l c") 'daedreth/copy-whole-line)
(global-set-key (kbd "C-c l k") 'kill-whole-line)

;; (with-eval-after-load "symbol-overlay"
;;   (global-set-key (kbd "C-; y") symbol-overlay-map))

(provide 'my-init-keybindings)
