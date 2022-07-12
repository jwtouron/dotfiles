;;;; Keybindings

(set-prefix-key (kbd "C-z"))

(define-key *root-map* (kbd "b") "browser")
(define-key *root-map* (kbd "c") "terminal")
(define-key *root-map* (kbd "v") "hsplit")
(define-key *root-map* (kbd "V") "version")

(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-k") "move-focus up")

(define-key *top-map* (kbd "s-L") "move-window right")
(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-K") "move-window up")

(define-key *top-map* (kbd "s-n") "pull-hidden-next")
(define-key *top-map* (kbd "s-p") "pull-hidden-previous")

(define-key *top-map* (kbd "s-RET") "terminal")
(define-key *top-map* (kbd "s-'") "windowlist")
(define-key *top-map* (kbd "s-\"") "windowlist")
