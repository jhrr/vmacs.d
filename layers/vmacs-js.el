;;; vmacs-js.el -- Configure JavaScript. -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(use-package js
  :mode
  ("\\.[cm]?jsx?\\'" . js-mode)
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2))

(use-package css-mode
  :defer t
  :custom
  (css-indent-offset 2))

(provide 'vmacs-js)
;;; vmacs-js.el ends here
