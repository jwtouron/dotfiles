;; -*- lexical-binding: t; -*-

(defcustom my-font nil
  "My font, in the form (name . weight)"
  :type '(list string integer)
  :group 'my
  :set (lambda (var font)
         (set var font)
         (when font
           (set-face-attribute 'default nil :font (car font) :height (cdr font)))))

(provide 'my-defcustoms)
