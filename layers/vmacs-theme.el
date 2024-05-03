;;; vmacs-theme.el --- Colours, appearance, faces. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(set-face-attribute 'fringe nil :background nil)
(fringe-mode 10)

(use-package vmacs-labburn-theme
  :load-path vmacs-lisp
  :config
  (enable-theme 'vmacs-labburn))

(provide 'vmacs-theme)
;;; vmacs-theme.el ends here
