;;; vmacs-lisp.el --- Configure lisps. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (defvar lisp-find-map)
  (define-prefix-command #'lisp-find-map)

  (bind-key "C-h e" #'lisp-find-map)
  (bind-key "C-h e c" #'finder-commentary)
  (bind-key "C-h e e" #'view-echo-area-messages)
  (bind-key "C-h e f" #'find-function)
  (bind-key "C-h e F" #'find-face-definition)
  (bind-key "C-h e i" #'info-apropos)
  (bind-key "C-h e k" #'find-function-on-key)
  (bind-key "C-h e l" #'find-library)
  (bind-key "C-h e s" #'scratch)
  (bind-key "C-h e v" #'find-variable)
  (bind-key "C-h e V" #'apropos-value)

  (defvar lisp-modes
    '(emacs-lisp-mode
      ielm-mode
      inferior-lisp-mode
      inferior-emacs-lisp-mode
      lisp-interaction-mode
      lisp-mode
      sly-mode
      sly-mrepl-mode)))

(defvar lisp-mode-hooks
  (seq-map
   (lambda (mode) (intern (concat (symbol-name mode) "-hook")))
   lisp-modes))

(seq-do (lambda (mode)
          (font-lock-add-keywords
           mode
           '(("(\\(lambda\\)\\>"
              (0 (ignore
                  (compose-region (match-beginning 1)
                                  (match-end 1) ?λ))))
             ("(\\|)" . 'dim-parens-face)
             ("(\\(ert-deftest\\)\\>[ '(]*\\(setf[ ]+\\sw+\\|\\sw+\\)?"
              (1 font-lock-keyword-face)
              (2 font-lock-function-name-face nil t)))))
        lisp-modes)

(use-package lisp-mode
  :defer t
  :mode-hydra
  ((lisp-mode emacs-lisp-mode sly-mode sly-mrepl-mode)
   ("Sexp"
    (("c" #'comment-or-uncomment-sexp "comment")
     ("m" #'mark-sexp "mark")
     ("x" #'eval-last-sexp "eval last")
     ("D" #'kill-sexp "kill")
     ("X" #'macrostep-expand "macrostep expand"))
    "Paredit"
    (("d" #'paredit-kill "kill")
     ("h" #'paredit-backward-up "backward up")
     ("j" #'paredit-forward "forward")
     ("k" #'paredit-backward "backward")
     ("l" #'paredit-forward-up "forward up")
     ("n" #'paredit-newline "newline")
     ("o" #'paredit-split-sexp "split")
     ("p" #'paredit-splice-sexp "splice")
     ("v" #'paredit-convolute-sexp "convolute")
     ("w" #'paredit-wrap-round "wrap")
     ("J" #'paredit-join-sexps "join")
     ("R" #'paredit-raise-sexp "raise")
     ("bb" #'paredit-backward-barf-sexp "barf backwards")
     ("bf" #'paredit-forward-barf-sexp "barf forwards")
     ("sb" #'paredit-backward-slurp-sexp "slurp backwards")
     ("sf" #'paredit-forward-slurp-sexp "slurp forwards"))
    "REPL"
    (("J" #'sly "sly")
     ("S" #'sly-scratch "sly scratch")
     (":" #'eval-expression "eval lisp expression"))
    "Find"
    (("ec" #'finder-commentary "finder-commentary")
     ("ef" #'find-function "find-function")
     ("eF" #'find-face-definition "find-face-definition")
     ("ei" #'info-apropos "info-apropos")
     ("ek" #'find-function-on-key "find-function-on-key")
     ("el" #'find-library "find-library")
     ("es" #'scratch "scratch")
     ("ev" #'find-variable "find-variable")
     ("eV" #'apropos-value "apropos-value"))))
  :preface
  (defun scratch ()
    "Jump to the scratch buffer."
    (interactive)
    (let ((current-mode major-mode))
      (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (goto-char (point-min))
      (when (looking-at ";")
        (forward-line 4)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (if (memq current-mode lisp-modes)
          (funcall current-mode))))

  (defun uncomment-sexp (&optional n)
    "Uncomment a sexp, or N sexps, around point."
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
        ;; ...and everything after it.
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
        (comment-sexp--raw)))))

(use-package macrostep
  :straight t
  :commands
  (macrostep-expand))

(use-package paredit
  :straight t
  :commands
  (paredit-mode)
  :init
  (seq-map
   (lambda (hook) (progn
               (add-hook hook 'paredit-mode)
               (add-hook hook 'evil-paredit-mode)))
   lisp-mode-hooks))

(use-package sly
  :straight t
  :commands
  (sly sly-scratch)
  :init
  (require 'sly-autoloads)
  (setq inferior-lisp-program "sbcl")
  (setq sly-net-coding-system 'utf-8-unix)
  (setq sly-contribs '(sly-fancy
                       sly-fancy-inspector
                       sly-macrostep
                       sly-mrepl
                       sly-repl-ansi-color
                       sly-scratch))
  :config
  (use-package sly-macrostep :straight t)
  (use-package sly-repl-ansi-color :straight t)

  (let ((map sly-editing-mode-map))
    (define-key map [remap display-local-help] #'sly-describe-symbol)
    (define-key map [remap embark-pp-eval-defun] #'sly-compile-defun)
    (define-key map [remap pp-macroexpand-expression] #'sly-expand-1)
    (define-key map [remap pp-eval-expression] #'sly-interactive-eval)
    (define-key map [remap xref-find-definitions] #'sly-edit-definition)))

(provide 'vmacs-lisp)
;;; vmacs-lisp.el ends here
