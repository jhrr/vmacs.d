;;; vmacs-edit.el --- Text editing. -*- lexical-binding:t -*-

;;; Commentary:

;; Enhance the overall text editing experience.

;;; Code:

(use-package evil :straight t)
(evil-mode)

(use-package aggressive-indent
  :straight t
  :hook
  (after-init . global-aggressive-indent-mode)
  :config
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
  :hook
  (after-init . smooth-scrolling-mode))

(provide 'vmacs-edit)
;;; vmacs-edit.el ends here
