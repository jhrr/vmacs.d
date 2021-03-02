;;; vmacs-edit.el ---  Enhance the text editing experience. -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(use-package aggressive-indent
  :straight t
  :hook
  (after-init . global-aggressive-indent-mode)
  :config
  (seq-do
   (lambda (mode) (add-to-list 'aggressive-indent-excluded-modes mode))
   '(org-mode)))

(use-package dumb-jump
  :straight t
  :init
  (setq dumb-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package rainbow-mode
  :straight t
  :config
  (define-globalized-minor-mode global-rainbow-mode rainbow-mode
    (lambda () (rainbow-mode)))
  (global-rainbow-mode))

(use-package smooth-scrolling
  :straight t
  :hook
  (after-init . smooth-scrolling-mode))

(provide 'vmacs-edit)
;;; vmacs-edit.el ends here
