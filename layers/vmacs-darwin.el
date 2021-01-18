;;; vmacs-darwin.el -*- lexical-binding:t -*-

;;; Commentary: macOS configuration.

;;; Code:

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . nil))

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

;; Add fonts to list in order of preference.
(set-face-attribute
 'default nil
 :font (font-candidate
	'"Inconsolata LGC 11"
	;; '"Source Code Pro 11"
	))

(defun insert-hash () (interactive) (insert "#"))
(global-set-key "\263" 'insert-hash)

(setq ring-bell-function #'ignore)
(setq frame-resize-pixelwise t)
(setq ns-use-proxy-icon nil)

(provide 'vmacs-darwin)
;;; vmacs-darwin.el ends here
