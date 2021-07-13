;;; vmacs-evil.el --- Vim emulation. -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(use-package evil
  :straight t
  :hook
  (after-init . evil-mode)
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1)
  ;; (define-key evil-normal-state-map ";" #'consult-buffer)
  )

(use-package evil-motion-trainer
  :straight
  (evil-motion-trainer
   :type git
   :host github
   :repo "martinbaillie/evil-motion-trainer")
  :after evil
  :config
  (global-evil-motion-trainer-mode 1))

(use-package evil-nerd-commenter
  :straight t
  :defer t
  :after evil
  :commands
  (evilnc-comment-or-uncomment-lines
   evilnc-comment-or-uncomment-paragraphs))

(provide 'vmacs-evil)
;;; vmacs-evil.el ends here
