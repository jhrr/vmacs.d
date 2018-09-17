;;; vmacs-ido.el -*- lexical-binding:t -*-

;;; Commentary: Configure ido.

;;; Code:

; TODO: https://github.com/DarwinAwardWinner/ido-completing-read-plus#smex

(eval-when-compile
  (require 'use-package))

(use-package ido
  :init
  (progn
    (ido-mode 1)
    (ido-everywhere 1))
  :config
  (progn
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-auto-merge-work-directories-length nil
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-use-virtual-buffers t
          ido-handle-duplicate-virtual-buffers 2
          ido-max-prospects 12)

    ; TODO: probably want to move these to their respective package defs.
    ; (setq magit-completing-read-function 'magit-ido-completing-read)
    ; (setq gnus-completing-read-function 'gnus-ido-completing-read)

    (use-package ido-completing-read+
      :straight t
      :init (ido-ubiquitous-mode 1))

    (use-package flx-ido
      :straight t
      :init
      (flx-ido-mode 1)
      :config
      (progn
        (setq ido-use-faces nil)
        (setq flx-ido-threshold 20000)))

    ; (use-package ido-springboard
    ;   :straight t)
    ))

(provide 'vmacs-ido)
;;; vmacs-ido.el ends here
