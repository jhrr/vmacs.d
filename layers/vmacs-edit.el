;;; vmacs-edit.el -*- lexical-binding:t -*-

;;; Commentary: Packages that enhance the editing experience.

;;; Code:

; (use-package linum-off
;   :straight t)

; (use-package smooth-scrolling
;   :straight t
;   :config (smooth-scrolling-mode t))

;; TODO: try this...
; (setq scroll-preserve-screen-position t)
; (setq scroll-margin 0)
; (setq scroll-conservatively 101)

; (use-package volatile-highlights
;   :straight t
;   :config (volatile-highlights-mode t))

; (use-package aggressive-indent
;   :straight t
;   :commands (global-aggressive-indent-mode)
;   :hook (prog-mode . (lambda () (require 'aggressive-indent)))
;   :init
;   (general-setq aggressive-indent-excluded-modes '())
;   :preface
;   (defun turn-off-aggressive-indent-mode ()
;     (when (fboundp 'aggressive-indent-mode)
;       (aggressive-indent-mode -1)))

;   :config
;   (progn
;     (add-hook 'diff-auto-refine-mode-hook #'turn-off-aggressive-indent-mode)
;     (global-aggressive-indent-mode t)))


(provide 'vmacs-edit)
;;; vmacs-edit.el ends here
