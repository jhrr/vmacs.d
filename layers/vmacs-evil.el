;;; vmacs-evil.el --- Vim emulation.

;;; Commentary:

;;; Code:

(use-package evil
  :straight t
  :init
  (setq evil-want-C-i-jump nil)
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1)
  ;; (define-key evil-normal-state-map ";" #'consult-buffer)
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

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

(provide 'vmacs-evil)
;;; vmacs-evil.el ends here
