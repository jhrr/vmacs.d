;;; vmacs-menus.el --- Menus and navigation -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; TODO: https://github.com/minad/vertico
;; https://github.com/minad/vertico/wiki/Migrating-from-Selectrum-to-Vertico
(use-package selectrum
  :straight t
  :init
  (selectrum-mode)
  :config
  (setq selectrum-num-candidates-displayed 30)
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)

  ;; TODO: vertico - https://github.com/radian-software/prescient.el
  (use-package selectrum-prescient
    :straight t
    :init
    (selectrum-prescient-mode)
    (prescient-persist-mode)
    (setq prescient-history-length 1000))

  (use-package consult
    :straight t
    :config
    (setq register-preview-delay 0
          register-preview-function #'consult-register-preview)

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

  (use-package mini-frame
    :straight t
    :init
    (mini-frame-mode)
    :custom
    (add-to-list 'mini-frame-ignore-commands "vr/*")
    (mini-frame-show-parameters '((top . 70)
                                  (width . 0.7)
                                  (left . 0.5))))

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
                (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
    (setq marginalia-annotators
          '(marginalia-annotators-heavy marginalia-annotators-light nil)))

  (use-package orderless
    :straight t
    :init (icomplete-mode)
    :custom (completion-styles '(orderless))))

(use-package which-key
  :straight t
  :init
  (which-key-mode))

(defun git-files ()
  "Jump to a file in the git tree."
  (interactive)
  (if (under-vc-p)
      (find-file
       (f-expand
        (let ((selectrum-should-sort-p nil))
          (completing-read
           "Open tracked file: "
           (magit-git-lines "-C" (magit-git-dir) "ls-files" "--full-name")))
        (vc-root-dir)))
    (message "Not under vc: %s" buffer-file-name)))

(defun git-modified-files ()
  "Jump to a modified file in the git tree."
  (interactive)
  (if (under-vc-p)
      (find-file
       (f-expand
        (let ((selectrum-should-sort-p nil))
          (completing-read
           "Open modified file: "
           (magit-git-lines "ls-files" "--full-name" "-m")))
        (vc-root-dir)))
    (message "Not under vc: %s" buffer-file-name)))

(provide 'vmacs-menus)
;;; vmacs-menus.el ends here
