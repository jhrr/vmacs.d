;;; vmacs-lisp.el -*- lexical-binding:t -*-

;;; Commentary: Lisp mode configuration.

;;; Code:

;; (defvar lisp-find-map)
;; (define-prefix-command #'lisp-find-map)
;; (bind-key "C-h e" #'lisp-find-map)
;; (bind-key "C-h e c" #'finder-commentary)
;; (bind-key "C-h e e" #'view-echo-area-messages)
;; (bind-key "C-h e f" #'find-function)
;; (bind-key "C-h e F" #'find-face-definition)
;; (bind-key "C-h e i" #'info-apropos)
;; (bind-key "C-h e k" #'find-function-on-key)
;; (bind-key "C-h e l" #'find-library)
;; (bind-key "C-h e s" #'scratch)
;; (bind-key "C-h e v" #'find-variable)
;; (bind-key "C-h e V" #'apropos-value)

;; (eval-after-load "slime"
;;   '(progn
;;      (setq common-lisp-hyperspec-root
;;            "/usr/local/share/doc/hyperspec/HyperSpec/")
;;      (setq common-lisp-hyperspec-symbol-table
;;            (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
;;      (setq common-lisp-hyperspec-issuex-table
;;            (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))))

;; (advice-add 'slime-display-or-scroll-completions :around
;;              (defun my--slime-completion-in-region (_ completions start end)
;;                (completion-in-region start end completions)))

(provide 'vmacs-lisp)
;;; vmacs-lisp.el ends here
