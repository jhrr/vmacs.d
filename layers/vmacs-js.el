;;; vmacs-js.el --- Configure JavaScript.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package js
  :preface
  (defun configure-js ()
    (setopt js-indent-level 2)
    (setopt js-switch-indent-offset 2))
  :hook
  ((js-mode . configure-js)
   (js-mode . js2-minor-mode)))

(use-package js2-mode
  :straight t
  :commands
  (js2-minor-mode))

(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :config
  (use-package json-reformat
    :straight t
    :after json-mode))

(use-package css-mode
  :mode "\\.css\\'"
  :custom
  (css-indent-offset 2))

(provide 'vmacs-js)
;;; vmacs-js.el ends here
