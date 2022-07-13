;;;; Keybindings

(set-prefix-key (kbd "C-z"))

(define-key *root-map* (kbd "b") "browser")
(define-key *root-map* (kbd "c") "terminal")
(define-key *root-map* (kbd "q") "powermenu")
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
(define-key *top-map* (kbd "s-'") "pull-from-windowlist")
(define-key *top-map* (kbd "s-\"") "pull-from-windowlist")

(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-5") "gselect 5")
(define-key *top-map* (kbd "s-6") "gselect 6")
(define-key *top-map* (kbd "s-7") "gselect 7")
(define-key *top-map* (kbd "s-8") "gselect 8")
(define-key *top-map* (kbd "s-9") "gselect 9")
(define-key *top-map* (kbd "s-0") "gselect 10")

(define-key *top-map* (kbd "s-!") "gmove 1")
(define-key *top-map* (kbd "s-@") "gmove 2")
(define-key *top-map* (kbd "s-#") "gmove 3")
(define-key *top-map* (kbd "s-$") "gmove 4")
(define-key *top-map* (kbd "s-%") "gmove 5")
(define-key *top-map* (kbd "s-^") "gmove 6")
(define-key *top-map* (kbd "s-&") "gmove 7")
(define-key *top-map* (kbd "s-*") "gmove 8")
(define-key *top-map* (kbd "s-(") "gmove 9")
(define-key *top-map* (kbd "s-)") "gmove 10")

(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-m") "mode-line")

;;; Requires stump-volume-control contrib module
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "s-]") "volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "s-[") "volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "volume-toggle-mute")
