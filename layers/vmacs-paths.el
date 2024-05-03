;;; vmacs-paths.el --- Directory and load path configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package emacs
  :init
  (defalias 'file-as-dir 'file-name-as-directory)

  (defun add-to-load-path (path &optional dir)
    (setq load-path
          (cons
           (expand-file-name path (or dir user-emacs-directory)) load-path)))

  (defmacro defpath (sym path &optional root add-path)
    "Define a subfolder of the `user-emacs-directory' or ROOT.
SYM is declared as a special variable set to PATH. This directory
will be created if it doesn't exist and added to the `load-path' if
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
    (concat (file-as-dir user-home-directory) "Dropbox/"))
  (defpath layers "layers" nil t)
  (defpath lisp "lisp" nil t)
  (defpath caches ".caches")
  (defpath autosaves "auto-save-list" (file-as-dir caches))
  (defpath backups "backups" (file-as-dir caches))

  (setq auto-save-list-file-prefix (concat (file-as-dir autosaves) ".auto-save-")
        auto-save-file-name-transforms `((".*" ,autosaves t))
        backup-directory-alist '(("." . "~/.emacs.d/.caches/backups"))
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 5
        kept-old-versions 2
        version-control t)

  (let ((custom-path (expand-file-name "custom.el" user-emacs-directory)))
    (unless (file-exists-p custom-path)
      (message "Creating %s because it doesn't exist..." custom-path)
      (shell-command (concat "touch " (shell-quote-argument custom-path))))
    (setq custom-file custom-path)
    (load custom-file))

  (add-to-list 'custom-theme-load-path layers)

  (defun hash-keys (hash)
    "Return all the keys in a HASH."
    (let ((keys ()))
      (maphash (lambda (k _v) (push k keys)) hash)
      (reverse keys)))

  (defun filename-map (filenames)
    "Hash FILENAMES mapped to their paths."
    (let ((filename-hash (make-hash-table :test 'equal)))
      (seq-map (lambda (path)
                 (puthash
                  (file-name-base path)
                  path filename-hash))
               filenames)
      filename-hash))

  (defun directory-el (dir)
    "Return all elisp files under a DIR."
    (seq-filter
     (lambda (filename) (not (string-prefix-p ".#" filename)))
     (directory-files dir t "\.el$" nil)))

  (defun jump-to-file (map)
    "Jump to a file via a MAP produced by `filename-map'."
    (find-file
     (gethash (completing-read "Open file: " (hash-keys map)) map)))

  (defun jump-to-init ()
    "Edit the `user-init-file', in another window."
    (interactive)
    (find-file user-init-file))
  (bind-key* "C-c I" 'jump-to-init)

  (defun jump-to-layer ()
    "Jump to a selected layer file."
    (interactive)
    (jump-to-file (filename-map (directory-el layers))))
  (bind-key* "C-c L" 'jump-to-layer)

  (defconst vmacs-features
    (seq-concatenate 'list (directory-el layers) (directory-el lisp))))

(provide 'vmacs-paths)
;;; vmacs-paths.el ends here
