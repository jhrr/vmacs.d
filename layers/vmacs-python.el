;;; vmacs-python.el --- Configure Python. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package python
  :straight t
  :mode ("\\.py\\'" . python-mode)
  :mode-hydra
  (python-mode
   ("Eval"
    (("J" #'run-python "repl")
     ("b" #'python-shell-send-buffer "buffer")
     ("f" #'python-shell-send-file "file")
     ("v" #'pyvenv-activate "activate venv")
     ("w" #'pyvenv-workon "workon"))
    "Test"
    (("t" #'pytest-all "all")
     ("T" #'pytest-one "one")
     ("Tm" #'pytest-module "module")
     ("Ts" #'pytest-suite "suite"))))
  :preface
  (defun init-python-mode ()
    (setq-local comment-inline-offset 2)
    (setq-local tab-width 4)
    (setq-local evil-shift-width 4)
    (setq python-indent-guess-indent-offset nil)
    (setq python-indent-offset 4)
    (setq python-fill-docstring-style 'pep-257)
    (setq python-shell-completion-native-enable nil)
    (prettify-symbols-mode -1)
    (when (executable-find "ipython")
      (setq-local python-shell-interpreter "ipython")
      (setq-local python-shell-interpreter-args "--simple-prompt -i")))
  :hook (python-mode . init-python-mode)
  :config
  (use-package pytest :straight t))

(provide 'vmacs-python)
;;; vmacs-python.el ends here
