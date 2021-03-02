;;; vmacs-search.el --- Configure greppage and findage. -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(defun find-project-todo-file ()
  "Open up the nearest TODO.org file, if it exists."
  (interactive)
  (let ((todo-path
         (locate-dominating-file (or (buffer-file-name) "") "TODO.org")))
    (if todo-path
        (find-file (concat todo-path "TODO.org"))
      (message "No suitable `TODO.org' candidate found."))))

(use-package visual-regexp
  :straight t
  :commands
  (vr/replace vr/query-replace))

(provide 'vmacs-search)
;;; vmacs-search.el ends here
