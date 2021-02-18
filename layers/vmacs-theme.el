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

(defface dim-parens-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to tastefully dim parentheses."
  :group 'vmacs)

(deftheme vmacs
  "Custom theme for vmacs.")

(defconst vmacs-palette
  '((ansi-0 . "#8f9ca0")
    (ansi-1 . "#c15064")
    (ansi-2 . "#89a97d")
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
    (bold . "#eff0ef")
    (comments . "#79ab87")
    (cursor . "#839495")
    (cursor-guide . "#b3ecff")
    (cursor-text . "#003440")
    (foreground . "#dadfe0")
    (link . "#005bbb")
    (keyword . "#f5d99a")
    (selected-text . "#505052")
    (selection-colour . "#e9edf1")))

;; TODO: (list-faces-display)
(let-alist vmacs-palette
  (let ((class '((class color) (min-colors 89))))
    (custom-theme-set-faces
     'vmacs
     `(default ((,class (:foreground ,.foreground :background ,.background))))
     `(cursor  ((,class (:background ,.cursor))))

     `(font-lock-builtin-face ((,class (:foreground ,.keyword :background ,.background))))
     `(font-lock-comment-face ((,class (:foreground ,.comments :background ,.background))))
     `(font-lock-comment-delimiter-face ((,class (:foreground ,.comments :background ,.background))))
     `(font-lock-constant-face ((,class (:foreground ,.foreground :background ,.background))))
     `(font-lock-doc-face ((,class (:foreground ,.comments :background ,.background))))
     `(font-lock-function-name-face ((,class (:foreground ,.foreground :background ,.background))))
     `(font-lock-keyword-face ((,class (:foreground ,.foreground :background ,.background))))
     `(font-lock-string-face ((,class (:foreground ,.comments :background ,.background))))
     `(font-lock-type-face ((,class (:foreground ,.foreground :background ,.background))))
     `(font-lock-variable-name-face ((,class (:foreground ,.foreground :background ,.background))))
     `(show-paren-match ((,class (:foreground ,.bold))))

     `(selectrum-current-candidate ((,class (:foreground ,.keyword :underline t :weight bold))))

     )))


(set-face-attribute 'fringe nil :background nil)
(fringe-mode 10)

(bind-key* "C-. d" #'describe-face)

(provide-theme 'vmacs)
(provide 'vmacs-theme)
;;; vmacs-theme.el ends here
