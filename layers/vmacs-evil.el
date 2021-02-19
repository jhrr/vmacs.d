;;; vmacs-evil.el --- Vim emulation. -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(use-package evil
  :straight t
  :hook
  (after-init . evil-mode)
  :config
  ;; TODO: Find a better way of dealing with this clash.
  (define-key evil-normal-state-map (kbd "C-.") nil))

(provide 'vmacs-evil)
;;; vmacs-evil.el ends here
