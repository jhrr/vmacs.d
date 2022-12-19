;;; vmacs-git.el --- Configure git and vcs utilties.

;;; Commentary:

;;; Code:

(use-package magit
  :straight t
  :custom (magit-git-executable "/usr/local/bin/git")
  :commands (magit-git-lines magit-git-dir magit-git-repo-p)
  :bind (("C-c g" . magit-status)
         ("C-c l" . magit-log))
  :config
  (progn
    (setq magit-display-buffer-function
          #'magit-display-buffer-fullframe-status-v1)
    (setq magit-completing-read-function #'selectrum-completing-read)))

(use-package git-auto-commit-mode :straight t)

(provide 'vmacs-git)
;;; vmacs-git.el ends here
