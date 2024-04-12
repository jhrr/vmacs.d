;;; vmacs-evil.el --- Vim emulation. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (setq key-chord-two-keys-delay 0.5)
  (setq-default cursor-in-non-selected-windows nil)
  (define-key evil-normal-state-map ";" #'consult-buffer)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1)
  (evil-mode))

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

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

(provide 'vmacs-evil)
;;; vmacs-evil.el ends here
