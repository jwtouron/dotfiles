(deftheme jake-at-linux
  "Created 2022-11-02.")

(let ((foreground "#c5c5be")
      (background "#1b1d1e")
      (cursor     "#c5c5be")

      (dark-purple  "#6e378a")
      (light-purple "#884fa5")
      (blue         "#5f87af")
      (dark-teal    "#007687")
      (light-teal   "#477d8f")

      (black "#1b1d1e")
      (white "#dadad5")

      (grey00 "#2f3234")  ; (let ((mul 1.75)) (format "%2x%2x%2x" (* mul #x1b) (* mul #x1d) (* mul #x1e)))
      (grey01 "#505354")
      (grey02 "#615f5e")
      (grey03 "#737271")
      (grey04 "#737074")
      (grey05 "#9a999d")
      (grey06 "#909495")
      (grey07 "#a2a2a5")
      (grey08 "#c5c5be")
      )

  (custom-theme-set-faces
   'jake-at-linux
   ;; built-in
   `(default ((t (:foreground ,foreground :background ,background))))
   `(cursor ((t (:background ,cursor))))
   `(font-lock-comment-face ((t (:slant italic :foreground ,grey02))))
   `(font-lock-constant-face ((t (:foreground ,dark-teal))))
   `(font-lock-function-name-face ((t (:foreground ,light-teal))))
   `(font-lock-keyword-face ((t (:foreground ,light-purple))))
   `(font-lock-string-face ((t (:foreground ,blue))))
   `(font-lock-type-face ((t (:foreground ,light-teal))))
   `(highlight ((t (:background ,grey00))))
   `(hl-line ((t (:background ,grey00))))
   `(minibuffer-prompt ((t (:foreground ,light-teal))))
   `(mode-line ((t (:background ,grey00 :overline ,grey04))))
   `(region ((t (:background ,grey01))))
   ))

(provide-theme 'jake-at-linux)
