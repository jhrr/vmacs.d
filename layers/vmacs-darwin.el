;;; vmacs-darwin.el --- Darwin/macOS configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package emacs
  :preface
  (declare-function font-candidate "vmacs-core")

  (defun insert-hash ()
    "Insert a hash character."
    (interactive)
    (insert "#"))

  (defun insert-euro ()
    "Insert a euro character."
    (interactive)
    (insert "€"))
  :init
  (add-to-list 'frameset-filter-alist '(ns-transparent-titlebar . :never))
  (add-to-list 'frameset-filter-alist '(ns-appearance . :never))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  (defvar face-overrides
    '(default
      fixed-pitch
      fixed-pitch-serif
      read-multiple-choice-face
      variable-pitch
      variable-pitch-text))

  (mapc
   (lambda (face)
     (set-face-attribute face nil :font (font-candidate '"Inconsolata LGC 12")))
   face-overrides)

  ;; TODO: This should be something else with unicode points.
  (set-face-attribute
   'glyphless-char nil
   :font (font-candidate '"Inconsolata LGC 12")))

(setq ring-bell-function #'ignore)
(setq frame-resize-pixelwise t)
(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "open")
(setq ns-use-proxy-icon nil)

(global-set-key "\263" #'insert-hash)
(global-set-key "\262" #'insert-euro)

(use-package exec-path-from-shell
  :straight t
  :if window-system
  :init
  (setq exec-path-from-shell-arguments nil)
  (defconst exec-path-from-shell-variables
    '("PATH"
      "MANPATH"
      "SSH_AGENT_PID"
      "GPG_TTY"
      "TEXINPUTS"
      "RUST_SRC_PATH"
      "VIRTUAL_ENV"))
  (exec-path-from-shell-initialize)
  (when-let* ((gls (executable-find "gls")))
    (setq insert-directory-program gls)))

(use-package osx-trash
  :straight t
  :preface
  (autoload 'osx-trash-setup "osx-trash")
  :config
  (osx-trash-setup)
  (setq delete-by-moving-to-trash t))

(provide 'vmacs-darwin)
;;; vmacs-darwin.el ends here
