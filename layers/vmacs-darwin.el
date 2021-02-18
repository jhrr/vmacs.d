;;; vmacs-darwin.el -- macOS configuration -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(add-to-list 'frameset-filter-alist '(ns-transparent-titlebar . :never))
(add-to-list 'frameset-filter-alist '(ns-appearance . :never))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Add fonts to list in order of preference.
(set-face-attribute
 'default nil
 :font (font-candidate
        '"Inconsolata LGC 11"
        ;; '"Source Code Pro 11"
        ))

(use-package exec-path-from-shell
  :straight t
  :if window-system
  :preface
  (declare-function exec-path-from-shell-initialize "vmacs-darwin")
  :init
  (progn
    (defvar exec-path-from-shell-arguments '("-l"))
    (defconst exec-path-from-shell-variables
      '("PATH"
        "MANPATH"
        "SSH_AGENT_PID"
        "GPG_TTY"
        "TEXINPUTS"
        "RUST_SRC_PATH"))
    (exec-path-from-shell-initialize)
    (when-let* ((gls (executable-find "gls")))
      (setq insert-directory-program gls))))

(use-package osx-trash
  :straight t
  :preface (autoload 'osx-trash-setup "osx-trash")
  :config
  (progn
    (osx-trash-setup)
    (setq delete-by-moving-to-trash t)))

(defun insert-hash ()
  "Insert a hash character."
  (interactive)
  (insert "#"))
(global-set-key "\263" 'insert-hash)

(setq ring-bell-function #'ignore)
(setq frame-resize-pixelwise t)
(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "open")
(setq ns-use-proxy-icon nil)

(provide 'vmacs-darwin)
;;; vmacs-darwin.el ends here
