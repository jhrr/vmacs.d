;;; vmacs-evil.el --- Vim emulation. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package evil
  :straight t
  :preface
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq key-chord-two-keys-delay 0.5)
  (setq-default cursor-in-non-selected-windows nil)
  :init
  (evil-mode)
  :config
  (define-key evil-normal-state-map ";" #'consult-buffer)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-motion-trainer
  :disabled t
  :straight
  (evil-motion-trainer
   :type git
   :host github
   :repo "martinbaillie/evil-motion-trainer")
  :after evil
  :config
  (global-evil-motion-trainer-mode 1))

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-paredit
  :straight t
  :after evil
  :commands
  (evil-paredit-mode))

(provide 'vmacs-evil)
;;; vmacs-evil.el ends here
