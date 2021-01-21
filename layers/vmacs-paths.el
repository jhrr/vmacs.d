;;; vmacs-paths.el -*- lexical-binding:t -*-

;;; Commentary: Directory and load path configuration.

;;; Code:

(defalias 'file-as-dir 'file-name-as-directory)

(defun add-to-load-path (path &optional dir)
  (setq load-path
        (cons
          (expand-file-name path (or dir user-emacs-directory)) load-path)))

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

(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (unless (file-exists-p custom-file)
    (message "Creating %s because it doesn't exist..." custom-file)
    (shell-command (concat "touch " (shell-quote-argument custom-file))))
  (setq custom-file custom-file))

(defun layers ()
  (seq-filter
   (lambda (path) (not (string-prefix-p ".#" path)))
   (directory-files layers t "\.el$" nil)))

(defun layers-map ()
  "Return a hash of layer names mapped to layer paths."
  (let ((layers-hash (make-hash-table :test 'equal)))
    (seq-map (lambda (path)
               (puthash
                 (file-name-base path)
                path layers-hash))
             (layers))
    layers-hash))

(defun jump-to-layer ()
  "Quickly jump to a config layer."
  (interactive)
  (let ((layers-map (layers-map)))
    (find-file
     (gethash
      (completing-read "Open layer file: " (hash-keys layers-map))
      layers-map))))
(bind-key* "C-c L" 'jump-to-layer)

(provide 'vmacs-paths)
;;; vmacs-paths.el ends here
