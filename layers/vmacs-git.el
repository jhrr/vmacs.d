;;; vmacs-git.el --- Configure git and vcs utilties. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package magit
  :straight t
  :bind
  (("C-c g" . magit-status)
   ("C-c l" . magit-log))
  :preface
  (defun under-vc-p ()
    "Determine if we are currently under version control."
    (magit-git-repo-p (or (vc-root-dir) "")))

  (defun git-files ()
    "Jump to a file in the git tree."
    (interactive)
    (if (under-vc-p)
        (find-file
         (f-expand
          (let ((vertico-should-sort-p nil))
            completing-read)
          (vc-root-dir)))
      (message "Not under vc: %s" buffer-file-name)))

  (defun git-modified-files ()
    "Jump to a modified file in the git tree."
    (interactive)
    (if (under-vc-p)
        (find-file
         (f-expand
          (completing-read
           "Open modified file: "
           (magit-git-lines "ls-files" "--full-name" "-m"))
          (vc-root-dir)))
      (message "Not under vc: %s" buffer-file-name)))

  :config
  ;; (setq magit-completing-read-function #'completing-read)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package git-auto-commit-mode :straight t)

(provide 'vmacs-git)
;;; vmacs-git.el ends here
