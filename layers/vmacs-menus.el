;;; vmacs-ido.el -*- lexical-binding:t -*-

;;; Commentary: Configure menus and navigation.

;;; Code:

(use-package selectrum
  :straight t
  :init (selectrum-mode 1)
  :config
  (progn
    (use-package consult
      :straight t)

    (use-package selectrum-prescient
      :straight t
      :config
      (progn
        (selectrum-prescient-mode 1)
        (prescient-persist-mode 1)))

    (use-package embark
      :straight t
      :bind ("C-M-a" . embark-act)
      :config
      (progn
        (use-package embark-consult
          :straight t
          :after (embark consult)
          :demand t
          :hook (embark-collect-mode . embark-consult-preview-minor-mode))))

    (use-package marginalia
      :straight t
      :bind (:map minibuffer-local-map ("C-s-a" . marginalia-cycle))
      :config (marginalia-mode))

    (use-package orderless
      :straight t
      :config (icomplete-mode)
      :custom (completion-styles '(orderless)))

    (setq selectrum-num-candidates-displayed 30)
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
    ; TODO: Probably better moved to magit.
    (setq magit-completing-read-function #'selectrum-completing-read)))

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
       '((top . 100)
         (width . 0.7)
         (left . 0.5))))))

(defun layers ()
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
