;;; vmacs-theme.el --- Colours, appearance, fontage. -*- lexical-binding:t -*-

;;; Commentary:

;;

;;; Code:

;; TODO: Highlight TODO face.

;; Highlight .value as if it were a :keyword.
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\_<\\.\\(?:\\sw\\|\\s_\\)+\\_>" 0
    font-lock-builtin-face)))

(deftheme vmacs
  "Custom theme for vmacs.")

(defconst vmacs-palette
  '((ansi-0 . "#8f9ca0")
    (ansi-1 . "#c15064")
    (ansi-2 . "#89a97d")
    (ansi-3 . "#f5d99a")
    (ansi-4 . "#88afd4")
    (ansi-5 . "#ca656d")
    (ansi-6 . "#51b4a9")
    (ansi-7 . "#dadfe0")
    (ansi-8 . "#999999")
    (ansi-9 . "#f76050")
    (ansi-10 . "#79ab87")
    (ansi-11 . "#ffc900")
    (ansi-12 . "#88afd4")
    (ansi-13 . "#e39f89")
    (ansi-14 . "#61afab")
    (ansi-15 . "#dadfe0")
    (background . "#393a3c")
    (badge . "#ff2600")
    (bold . "#eff0ef")
    (cursor . "#839495")
    (cursor-guide . "#b3ecff")
    (cursor-text . "#003440")
    (foreground . "#dadfe0")
    (link . "#005bbb")
    (selected-text . "#505052")
    (selection-colour . "#e9edf1")))

;; TODO: (list-faces-display)
(let-alist vmacs-palette
  (custom-theme-set-faces
   'vmacs
   `(default ((t (:foreground ,.foreground :background ,.background))))

   ))

(provide-theme 'vmacs)

(provide 'vmacs-theme)
;;; vmacs-theme.el ends here
