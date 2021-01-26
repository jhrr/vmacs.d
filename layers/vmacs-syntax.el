;;; vmacs-syntax.el -- Syntax checking. -*- lexical-binding:t -*-

;;; Commentary:

;; On-the-fly code linting and prose policing.

;;; Code:

;; TODO: https://github.com/sachac/artbollocks-mode

(use-package flycheck
  :straight t
  :custom
  (progn
    (flycheck-emacs-lisp-initialize-packages t)
    (flycheck-display-errors-delay 0.1))
  :config
  (progn
    (global-flycheck-mode)
    (flycheck-set-indication-mode 'left-margin)

    (add-to-list 'flycheck-checkers 'proselint)))

(provide 'vmacs-syntax)
;;; vmacs-syntax.el ends here
