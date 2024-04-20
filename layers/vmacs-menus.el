;;; vmacs-menus.el --- Menus and navigation -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package mini-frame
  :straight t
  :init
  (mini-frame-mode)
  :custom
  (add-to-list 'mini-frame-ignore-commands "vr/*")
  (mini-frame-show-parameters '((top . 70)
                                (width . 0.7)
                                (left . 0.5))))

;; TODO:
;; (setq selectrum-refine-candidates-function #'orderless-filter)
;; (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
(use-package vertico
  :straight
  (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  (setq vertico-count 30))

(use-package vertico-prescient
  :straight t
  :after
  (vertico)
  :init
  (vertico-prescient-mode)
  (prescient-persist-mode)

  (use-package emacs
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    (setq prescient-history-length 1000)
    (setq enable-recursive-minibuffers t)
    (setq read-extended-command-predicate #'command-completion-default-include-p)
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :straight t
  :config
  (setq consult-async-min-input 3)
  (setq register-preview-delay 0)
  (setq register-preview-function #'consult-register-preview)

  (advice-add #'register-preview :around
              (lambda (fun buffer &optional show-empty)
                (let ((register-alist
                       (seq-sort #'car-less-than-car register-alist)))
                  (funcall fun buffer show-empty))
                (when-let (win (get-buffer-window buffer))
                  (with-selected-window win
                    (setq-local mode-line-format nil)
                    (setq-local window-min-height 1)
                    (fit-window-to-buffer))))))

(use-package consult-flycheck
  :bind ("C-c x" . consult-flycheck)
  :straight t)

(use-package embark
  :straight t
  :bind ("C-c e" . embark-act))

(use-package embark-consult
  :straight t
  :after
  (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :straight t
  :bind
  (:map minibuffer-local-map ("C-c m" . marginalia-cycle))
  :init
  (marginalia-mode)
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p vertico-mode) (vertico--exhibit))))
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package orderless
  :straight t
  :init (icomplete-mode)
  :custom (completion-styles '(orderless)))

(use-package which-key
  :straight t
  :init
  (which-key-mode))

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(provide 'vmacs-menus)
;;; vmacs-menus.el ends here
