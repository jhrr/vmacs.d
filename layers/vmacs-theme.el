;;; vmacs-theme.el --- Colours, appearance, faces. -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(deftheme vmacs
  "Custom theme for vmacs.")

(defgroup vmacs-faces nil
  "Faces used by vmacs."
  :group 'faces)

(defface dim-parens-face
  '((t :inherit default))
  "Face used to tastefully dim parentheses."
  :group 'vmacs-faces)

(defface comment-todo-face
  '((t :inherit default))
  "Face to highlight TODOs in comments and strings."
  :group 'vmacs-faces)

(defconst vmacs-palette
  '((ansi-0 . "#8f9ca0")
    (ansi-1 . "#c15064")
    (ansi-2 . "#89a97d")
    (ansi-4 . "#88afd4")
    (ansi-6 . "#51b4a9")
    (ansi-8 . "#999999")
    (ansi-9 . "#f76050")
    (ansi-13 . "#e39f89")
    (ansi-14 . "#61afab")

    (background . "#393a3c")
    (blackish . "#242424")
    (bold . "#eff0ef")
    ;; (comments . "#79ab87")
    (comments . "#89a97d")
    (cursor . "#839495")
    (cursor-guide . "#b3ecff")
    (cursor-text . "#003440")
    (dim-parens-dark . "#7f7f7f")
    (dim-parens-light . "#8c8c8c")
    (foreground . "#dadfe0")
    (golden . "#ffc900")
    (link . "#88afd4")
    (string . "#ca656d")
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
     `(highlight ((,class (:foreground ,.background :background ,.selection-colour))))
     `(link ((,class (:foreground ,.link))))
     `(minibuffer-prompt ((,class (:foreground ,.link))))
     `(region ((,class (:background ,.blackish))))
     `(show-paren-match ((,class (:foreground ,.bold))))

     `(font-lock-builtin-face ((,class (:foreground ,.keyword))))
     `(font-lock-comment-face ((,class (:foreground ,.comments))))
     `(font-lock-comment-delimiter-face ((,class (:foreground ,.comments))))
     `(font-lock-constant-face ((,class (:foreground ,.foreground))))
     `(font-lock-doc-face ((,class (:foreground ,.comments))))
     `(font-lock-function-name-face ((,class (:foreground ,.foreground))))
     `(font-lock-keyword-face ((,class (:foreground ,.foreground))))
     `(font-lock-string-face ((,class (:foreground ,.string))))
     `(font-lock-type-face ((,class (:foreground ,.foreground))))
     `(font-lock-variable-name-face ((,class (:foreground ,.foreground))))

     `(marginalia-key ((,class (:foreground ,.keyword))))

     ;; https://github.com/tkf/org-mode/blob/master/lisp/org-faces.el
     `(org-ellipsis ((,class (:foreground ,.selection-colour))))
     `(org-target ((,class (:foreground ,.link))))
     ;; org-document-info-keyword
     ;; org-document-info
     ;; org-doument-title
     ;; org-meta-line
     ;; org-agenda-dimmed-todo-face -> https://github.com/tkf/org-mode/blob/c2ebeea6f68f2ef804d387c238e4acccf655dc64/lisp/org-faces.el#L632

     `(selectrum-current-candidate
       ((,class (:foreground ,.keyword :underline t :weight bold))))

     `(dim-parens-face ((,class (:foreground ,.dim-parens-dark))))
     `(comment-todo-face ((,class (:foreground ,.golden))))

     )))

(defun get-colour (colour)
  "Lookup a COLOUR (as a string) from VMACS-PALETTE."
  (cdr (assoc (intern colour) vmacs-palette)))

;; (use-package hl-todo
;;   :straight t
;;   :hook (after-init . global-hl-todo-mode)
;;   :config
;;   (setq hl-todo-keyword-faces
;;         '(("TODO"   . comment-todo-face)
;;           ("FIXME"  . comment-todo-face))))

;; Highlight .value as if it were a :keyword.
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\_<\\.\\(?:\\sw\\|\\s_\\)+\\_>" 0
    font-lock-builtin-face)))

(set-face-attribute 'fringe nil :background nil)
(fringe-mode 10)

(bind-key* "C-. d" #'describe-face)

(provide-theme 'vmacs)
(enable-theme 'vmacs)
(provide 'vmacs-theme)
;;; vmacs-theme.el ends here
