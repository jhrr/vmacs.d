;;; vmacs-menus.el --- Menus and navigation. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package posframe
  :disabled t
  :straight t)

(use-package hydra-posframe
  :disabled t
  :straight
  (hydra-posframe :type git :host github :repo "Ladicle/hydra-posframe")
  :hook
  (after-init . hydra-posframe-mode)
  :config
  (setq hydra-posframe-border-width 2))

(use-package vertico
  :straight
  (:files (:defaults "extensions/*"))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode)
  (setq vertico-count 30)
  (vertico-buffer-mode)
  (vertico-multiform-mode))

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
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :straight t
  :after
  (embark-consult)
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

(use-package embark-consult :straight t)

(use-package embark
  :straight t
  :after
  (embark-consult)
  :preface
  (defun with-minibuffer-keymap (keymap)
    (lambda (fn &rest args)
      (minibuffer-with-setup-hook
          (lambda ()
            (use-local-map
             (make-composed-keymap keymap (current-local-map))))
        (apply fn args))))

  (defvar embark-completing-read-prompter-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<tab>") 'abort-recursive-edit)
      map))

  (defun embark-act-with-completing-read (&optional arg)
    (interactive "P")
    (let* ((embark-prompter 'embark-completing-read-prompter)
           (embark-indicators '(embark-minimal-indicator)))
      (embark-act arg)))

  (advice-add 'embark-completing-read-prompter :around
              (with-minibuffer-keymap embark-completing-read-prompter-map))
  (define-key vertico-map (kbd "<backtab>") 'embark-act-with-completing-read)

  (bind-key* "C-c e" 'embark-act-with-completing-read)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package marginalia
  :straight t
  :bind
  (:map minibuffer-local-map
        ("C-c m" . marginalia-cycle))
  :init
  (marginalia-mode))

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
