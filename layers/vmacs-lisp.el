;;; vmacs-lisp.el -*- lexical-binding:t -*-

;;; Commentary: Lisp mode configuration.

;;; Code:

;; https://github.com/joaotavora/sly
;; https://github.com/abo-abo/lispy
;; macrostep

;; (defvar lisp-find-map)
;; (define-prefix-command #'lisp-find-map)
;; (bind-key "C-h e" #'lisp-find-map)
;; (bind-key "C-h e c" #'finder-commentary)
;; (bind-key "C-h e e" #'view-echo-area-messages)
;; (bind-key "C-h e f" #'find-function)
;; (bind-key "C-h e F" #'find-face-definition)
;; (bind-key "C-h e i" #'info-apropos)
;; (bind-key "C-h e k" #'find-function-on-key)
;; (bind-key "C-h e l" #'find-library)
;; (bind-key "C-h e s" #'scratch)
;; (bind-key "C-h e v" #'find-variable)
;; (bind-key "C-h e V" #'apropos-value)

;; (eval-after-load "slime"
;;   '(progn
;;      (setq common-lisp-hyperspec-root
;;            "/usr/local/share/doc/hyperspec/HyperSpec/")
;;      (setq common-lisp-hyperspec-symbol-table
;;            (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
;;      (setq common-lisp-hyperspec-issuex-table
;;            (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))))

;; (advice-add 'slime-display-or-scroll-completions :around
;;              (defun my--slime-completion-in-region (_ completions start end)
;;                (completion-in-region start end completions)))

(defvar lisp-modes
  '(ielm-mode
    lisp-mode
    emacs-lisp-mode
    slime-repl-mode
    inferior-lisp-mode
    inferior-emacs-lisp-mode
    lisp-interaction-mode))

(defvar lisp-mode-hooks
  (seq-map
   (lambda (mode) (intern (concat (symbol-name mode) "-hook")))
   lisp-modes))

(seq-do (lambda (major-mode)
        (font-lock-add-keywords
         major-mode
         '(("(\\(lambda\\)\\>"
            (0 (ignore
                (compose-region (match-beginning 1)
                                (match-end 1) ?λ))))
           ("(\\|)" . 'global-dim-paren-face)
           ("(\\(ert-deftest\\)\\>[ '(]*\\(setf[ ]+\\sw+\\|\\sw+\\)?"
            (1 font-lock-keyword-face)
            (2 font-lock-function-name-face
               nil t)))))
      lisp-modes)

(use-package highlight-cl
  :straight t
  :commands (highlight-cl-add-font-lock-keywords)
  :init
  (mapc (function
         (lambda (mode-hook)
           (add-hook mode-hook
                     'highlight-cl-add-font-lock-keywords)))
        lisp-mode-hooks))

(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (inhibit-field-text-motion t)
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (and (not (bobp))
                            (= end (save-excursion
                                     (comment-forward (point-max))
                                     (point))))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (ignore-errors
                  (while (looking-at-p comment-start-skip)
                    (forward-char -1)))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it...
      (ignore-errors
        (comment-region beg p))
      ;; ... and everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (eq 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave the point where it
    ;; was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (save-excursion
       (comment-region l r))
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

(bind-key* "C-. b" 'beginning-of-defun)
(bind-key* "C-. c" 'comment-or-uncomment-sexp)

(provide 'vmacs-lisp)
;;; vmacs-lisp.el ends here
