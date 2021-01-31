;;; vmacs-syntax.el -- Syntax checking. -*- lexical-binding:t -*-

;;; Commentary:

;; On-the-fly code linting and prose policing.

;;; Code:

(use-package flycheck
  :straight t
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)

  (add-to-list 'flycheck-checkers 'proselint))

(use-package artbollocks-mode
  :straight t
  :init (add-hook 'text-mode-hook 'artbollocks-mode))

(provide 'vmacs-syntax)
;;; vmacs-syntax.el ends here
