;;; vmacs-theme.el -*- lexical-binding:t -*-

;;; Commentary: Theme and appearance configuration.

;;; Code:

;; TODO: https://github.com/ksjogo/labburn-theme
;; TODO: https://github.com/anler/minimal-theme
;; TODO: https://github.com/edran/hc-zenburn-emacs

(defface dim-parens-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to tastefully dim parentheses.")

(provide 'vmacs-theme)
;;; vmacs-theme.el ends here
