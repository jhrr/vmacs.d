;;; vmacs-python.el --- Python mode. -*- lexical-binding:t -*-

;;; Commentary:

;; Mostly nicked from: https://github.com/chrisbarrett/.emacs.d/blob/master/config/config-python.el

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
  (autoload 'python-shell-get-process "python")

  (defun init-python-mode ()
    (setq-local comment-inline-offset 2)
    (setq-local tab-width 4)
    (setq-local evil-shift-width 4)
    (prettify-symbols-mode -1)
    (when (executable-find "ipython")
      (setq-local python-shell-interpreter "ipython")
      (setq-local python-shell-interpreter-args "--simple-prompt -i")))

  (defvar python-prev-source-buffer)

  (defun python-repl-switch-to-source ()
    (interactive)
    (-when-let (buf python-prev-source-buffer)
      (when (buffer-live-p buf)
        (pop-to-buffer buf))))

  (defun python-repl ()
    (interactive)
    (when (derived-mode-p 'python-mode)
      (setq python-prev-source-buffer (current-buffer)))
    (let ((shell-process
           (or (python-shell-get-process)
               (with-demoted-errors "Error: %S"
                 (call-interactively #'run-python)
                 (python-shell-get-process)))))
      (unless shell-process
        (error "Failed to start python shell properly"))
      (pop-to-buffer (process-buffer shell-process))
      (evil-insert-state)))
  :config
  (setq python-indent-guess-indent-offset nil)
  (setq python-indent-offset 4)
  (setq python-fill-docstring-style 'django)

  (define-key python-mode-map [remap python-shell-switch-to-shell] #'python-repl)
  (define-key inferior-python-mode-map (kbd "C-c C-z") #'python-repl-switch-to-source))

;; (use-package elpy
;;   :straight t
;;   :after python
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))

(use-package pytest
  :straight t
  :defer t
  :after python
  :config
  (add-to-list 'pytest-project-root-files "setup.cfg"))

(use-package pyvenv
  :straight t
  :commands (pyvenv-activate pyvenv-deactivate pyvenv-workon)
  :hook (python-mode . python-pyvenv-activate-if-found)
  :preface
  (progn
    (autoload 'f-join "f")
    (defvar python--venv-names '(".env" "env" ".venv" "venv" ".virtualenv"))

    (defun python--directory-first-ancestor (dir pred)
      (let (result)
        (while dir
          (pcase (funcall pred dir)
            (`nil
             (setq dir (f-parent dir)))
            (res
             (setq result res)
             (setq dir nil))))
        result))

    (defun python--find-venv-in-directory (dir)
      (-when-let ((dir) (--keep (let ((dir (f-join dir it)))
                                  (when (f-directory? dir)
                                    dir))
                                python--venv-names))
        (file-truename dir)))

    (defun python-pyvenv-dir ()
      (python--directory-first-ancestor default-directory
                                        #'python--find-venv-in-directory))

    (defun python-pyvenv-activate-if-found ()
      (-when-let (env (python-pyvenv-dir))
        (pyvenv-activate env)
        (message "Using pyvenv at %s" (f-abbrev env))))))

(use-package tox
  :straight t
  :defer t
  :after python
  :config
  (setq tox-runner "py.test"))

  (provide 'vmacs-python)
;;; vmacs-python.el ends here
