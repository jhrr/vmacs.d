;;; vmacs-syntax.el --- Syntax checking. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package flycheck
  :straight t
  :preface
  (flycheck-define-checker python-mypy
    "A Python type checker using mypy."
    :command ("mypy"
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line ": error:" (message) line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-mypy t)

  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'."
    :command ("ruff"
              "check"
              "--output-format=full"
              (eval (when buffer-file-name (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :modes python-mode
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end)))
  (add-to-list 'flycheck-checkers 'python-ruff t)

  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin)
  (add-to-list 'flycheck-checkers 'proselint))

(use-package reformatter
  :straight t
  :hook
  (python-mode . ruff-format-on-save-mode)
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-")))

;; TODO: eglot
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs '(python-mode . ("ruff-lsp"))))

(use-package artbollocks-mode
  :straight t
  :disabled t
  :init (add-hook 'text-mode-hook 'artbollocks-mode))

(provide 'vmacs-syntax)
;;; vmacs-syntax.el ends here
