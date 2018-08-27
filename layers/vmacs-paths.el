;;; vmacs-paths.el -*- lexical-binding:t -*-

;;; Commentary: Directory and load path configuration.

;;; Code:

(defalias 'file-as-dir 'file-name-as-directory)

(defun add-to-load-path (path &optional dir)
  (setq load-path
        (cons (expand-file-name path (or dir user-emacs-directory)) load-path)))

(defmacro defpath (sym path &optional root add-path)
  "Define a subfolder of the `user-emacs-directory' or ROOT.
SYM is declared as a special variable set to PATH. This directory
will be created if it doesn't exist and added to the load-path if
ADD-PATH is non-nil."
  `(defconst ,sym
     (let ((dir (concat (or ,root user-emacs-directory) ,path)))
       (unless (file-exists-p dir)
         (message "Creating directory: %s because it doesn't exist..." dir)
         (make-directory dir)
         (message "Created directory: %s..." dir))
       (when ,add-path (add-to-load-path dir))
       dir)))

(defconst user-home-directory (getenv "HOME"))
(defconst user-dropbox-directory
  (concat (file-as-dir user-home-directory) "Dropbox"))
(defpath vmacs/layers "layers" nil t)
(defpath vmacs/lisp "lisp" nil t)
(defpath vmacs/caches ".caches")
(defpath vmacs/autosaves "auto-save-list" (file-as-dir vmacs/caches))
(defpath vmacs/backups "backups" (file-as-dir vmacs/caches))

(setq auto-save-list-file-prefix (concat (file-as-dir vmacs/autosaves) ".auto-save-")
      auto-save-file-name-transforms `((".*" ,vmacs/autosaves t))
      backup-directory-alist '(("." . "~/.emacs.d/.caches/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 5
      kept-old-versions 2
      version-control t)

(let ((vmacs/custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (unless (file-exists-p vmacs/custom-file)
    (message "Creating %s because it doesn't exist..." vmacs/custom-file)
    (shell-command (concat "touch " (shell-quote-argument vmacs/custom-file))))
  (setq custom-file vmacs/custom-file))

(provide 'vmacs-paths)
;;; vmacs-paths.el ends here
