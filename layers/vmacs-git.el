;;; vmacs-git.el -*- lexical-binding:t -*-

;;; Commentary: Configure git utilties.

;;; Code:

(use-package magit
  :straight t
  :defer t
  :custom (magit-git-executable "/usr/local/bin/git")
  :bind (("C-c g" . magit-status)
         ("C-c l" . magit-log))
  :config
  (progn
    (setq magit-display-buffer-function
          #'magit-display-buffer-fullframe-status-v1)
    (setq magit-completing-read-function #'selectrum-completing-read)))

(provide 'vmacs-git)
;;; vmacs-git.el ends here
