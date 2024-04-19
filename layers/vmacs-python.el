;;; vmacs-python.el --- Configure Python. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package python
  :straight t
  :mode ("\\.py\\'" . python-mode)
  :mode-hydra
  (python-mode
   ("Eval"
    (("s" #'python-repl "shell")
     ("eb" #'python-shell-send-buffer "buffer")
     ("ef" #'python-shell-send-file "file"))
    "Test"
    (("ta" #'pytest-all "all")
     ("tt" #'pytest-one "one")
     ("tm" #'pytest-module "module")
     ("ts" #'pytest-suite "suite"))))
  :hook (python-mode . init-python-mode)
  :preface
  (defun init-python-mode ()
    (setq-local comment-inline-offset 2)
    (setq-local tab-width 4)
    (setq-local evil-shift-width 4)
    (prettify-symbols-mode -1)
    (when (executable-find "ipython")
      (setq-local python-shell-interpreter "ipython")
      (setq-local python-shell-interpreter-args "--simple-prompt -i")))
  :config
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4)
  (setq python-fill-docstring-style 'pep-257))

  (use-package pytest
    :straight t
    :defer t
    :after python)

(provide 'vmacs-python)
;;; vmacs-python.el ends here
