;;; vmacs-menus.el -*- lexical-binding:t -*-

;;; Commentary:

;; Configure menus and navigation.

;;; Code:

(use-package selectrum
  :straight t
  :init (selectrum-mode 1)
  :config
  (progn
    (use-package consult
      :straight t
      :bind (("C-. b" . consult-buffer)
             ("C-. w" . consult-buffer-other-window)
             ("C-. g" . consult-ripgrep))
      :init
      (progn
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
                          (fit-window-to-buffer)))))

        (defun git-files ()
          "Jump to a file in the git tree."
          (interactive)
          (if (under-vc-p)
              (find-file
               (f-expand
                (let ((selectrum-should-sort-p nil))
                  (completing-read
                   "Open tracked file: "
                   (magit-git-lines "-C" (magit-git-dir) "ls-files" "--full-name"))
                  (vc-root-dir))))
            (message "Not under vc: %s" buffer-file-name)))
        (bind-key* "C-. j" 'git-files)

        (defun git-modified-files ()
          "Jump to a modified file in the git tree."
          (interactive)
          (if (under-vc-p)
              (find-file
               (f-expand
                (let ((selectrum-should-sort-p nil))
                  (completing-read
                   "Open modified file: "
                   (magit-git-lines "ls-files" "--full-name" "-m"))
                  (vc-root-dir))))
            (message "Not under vc: %s" buffer-file-name)))
        (bind-key* "C-. f" 'git-modified-files)))

    (use-package consult-flycheck
      :straight t
      :bind ("C-. x" . consult-flycheck))

    (use-package selectrum-prescient
      :straight t
      :init
      (progn
        (selectrum-prescient-mode 1)
        (prescient-persist-mode 1)))

    (use-package embark
      :straight t
      :bind ("C-. e" . embark-act))

    (use-package embark-consult
      :straight t
      :after (embark consult)
      :demand t
      :hook (embark-collect-mode . embark-consult-preview-minor-mode))

    (use-package marginalia
      :straight t
      :bind (:map minibuffer-local-map ("C-. m" . marginalia-cycle))
      :init
      (progn
        (marginalia-mode)
        (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
        (setq marginalia-annotators
              '(marginalia-annotators-heavy marginalia-annotators-light nil))))

    (use-package orderless
      :straight t
      :init (icomplete-mode)
      :custom (completion-styles '(orderless)))

    (setq selectrum-num-candidates-displayed 30)
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)))

;; TODO: Won't need this when emacs 27.2 comes out?
(use-package mini-frame
  :straight t
  :init
  (progn
    ;; TODO: At least we definitely won't need this.
    (define-advice fit-frame-to-buffer (:around (f &rest args) dont-skip-ws-for-mini-frame)
      (cl-letf* ((orig (symbol-function #'window-text-pixel-size))
                 ((symbol-function #'window-text-pixel-size)
                  (lambda (win from to &rest args)
                    (apply orig
                           (append (list win from
                                         (if (and (window-minibuffer-p win)
                                                  (frame-root-window-p win)
                                                  (eq t to))
                                             nil
                                           to))
                                   args)))))
        (apply f args)))
    (mini-frame-mode 1)
    (custom-set-variables
     '(mini-frame-show-parameters
       '((top . 70)
         (width . 0.7)
         (left . 0.5))))))

(use-package which-key
  :straight t
  :init (which-key-mode))

(defun layers ()
  "List all layer files."
  (seq-filter
   (lambda (path) (not (string-prefix-p ".#" path)))
   (directory-files layers t "\.el$" nil)))

(defun layers-map ()
  "Return a hash of layer names mapped to layer paths."
  (let ((layers-hash (make-hash-table :test 'equal)))
    (seq-map (lambda (path)
               (puthash
                (file-name-sans-extension
                 (file-name-nondirectory path))
                path layers-hash))
             (layers))
    layers-hash))

(defun jump-to-layer ()
  "Quickly jump to a config layer."
  (interactive)
  (let ((layers-map (layers-map)))
    (find-file
     (gethash
      (completing-read "Open layer file: " (hash-keys layers-map))
      layers-map))))
(bind-key* "C-c L" 'jump-to-layer)

(provide 'vmacs-menus)
;;; vmacs-menus.el ends here
