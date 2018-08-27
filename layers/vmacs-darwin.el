;;; vmacs-darwin.el -*- lexical-binding:t -*-

;;; Commentary: macOS configuration.

;;; Code:

; (when (memq window-system '(mac ns))
;   (use-package exec-path-from-shell
;     :config
;     (progn
;       (setq exec-path-from-shell-check-startup-files nil)
;       (exec-path-from-shell-copy-env "PATH"))))

(set-face-attribute
 'default nil
 :font (font-candidate
	;; Add fonts here in order of preference.
	'"Inconsolata LGC 11"
	;; '"Source Code Pro 11"
	))

(setq ring-bell-function #'ignore)
(setq frame-resize-pixelwise t)
(toggle-frame-maximized)

(setq delete-by-moving-to-trash t)

(defun insert-hash ()
  (interactive)
  (insert "#"))

(global-set-key "\263" 'insert-hash)

(provide 'vmacs-darwin)
;;; vmacs-darwin.el ends here
