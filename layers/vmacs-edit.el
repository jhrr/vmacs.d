;;; vmacs-edit.el --- Text editing packages. -*- lexical-binding:t -*-

;;; Commentary:

;; Enhance the text editing experience.

;;; Code:

(use-package aggressive-indent
  :straight t
  :config
  (global-aggressive-indent-mode)
  (seq-do
   (lambda (mode) (add-to-list 'aggressive-indent-excluded-modes mode))
   '(org-mode)))

(use-package rainbow-mode
  :straight t
  :config
  (define-globalized-minor-mode global-rainbow-mode rainbow-mode
    (lambda () (rainbow-mode)))
  (global-rainbow-mode))

(use-package smooth-scrolling
  :straight t
  :config (smooth-scrolling-mode))

(provide 'vmacs-edit)
;;; vmacs-edit.el ends here
