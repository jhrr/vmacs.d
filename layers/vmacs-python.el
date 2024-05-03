;;; vmacs-python.el --- Configure Python. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(declare-function f-exists? "init")
(declare-function f-expand "init")
(declare-function f-traverse-upwards "init")

(defconst venv-dir ".venv")

(use-package pyvenv
  :straight t
  :commands
  (pyvenv-activate)
  :preface
  (defun pyvenv-autoload ()
    "Automatically activates a virtualenv if a `.venv' directory exists."
    (when (string-match ".*\.py$" (buffer-file-name))
      (f-traverse-upwards
       (lambda (path)
         (let ((venv-path (f-expand venv-dir path)))
           (message venv-path)
           (if (f-directory? venv-path)
               (progn
                 (pyvenv-activate venv-path)
                 (message "Activated venv: %s" venv-path))
             nil)))
       default-directory))))

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
     ("To" #'pytest-one "one")
     ("Tm" #'pytest-module "module")
     ("Ts" #'pytest-suite "suite"))))
  :preface
  (defun configure-python ()
    (setq-local comment-inline-offset 2)
    (setq-local tab-width 4)
    (setq-local evil-shift-width 4)
    (setq-local flycheck-checkers '(python-ruff))
    (setq python-indent-guess-indent-offset nil)
    (setq python-indent-offset 4)
    (setq python-fill-docstring-style 'pep-257)
    (setq python-shell-completion-native-enable nil)
    (prettify-symbols-mode nil)
    (when (executable-find "ipython")
      (setq-local python-shell-interpreter "ipython")
      (setq-local python-shell-interpreter-args "--simple-prompt -i"))
    (add-function :after after-focus-change-function (lambda () (pyvenv-autoload)))
    (pyvenv-autoload))
  :hook
  (python-mode . configure-python))

(use-package pytest
  :straight t
  :commands
  (pytest-all pytest-one pytest-module pytest-suite))

(provide 'vmacs-python)
;;; vmacs-python.el ends here
