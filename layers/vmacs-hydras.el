;;; vmacs-org.el -- Global hydras. -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(eval-and-compile
  (defun hydra-title-with-octicon (icon title)
    (concat (all-the-icons-octicon icon) " " title))

  (defun hydra-title-with-fileicon (icon title)
    (concat (all-the-icons-fileicon icon) " " title))

  (defun hydra-title-with-mode-icon (mode title)
    (concat (all-the-icons-icon-for-mode mode) " " title))

  (defun hydra-title-with-faicon (icon title)
    (concat (all-the-icons-faicon icon) " " title))

  (defun hydra-title-with-aicon (icon title)
    (concat (all-the-icons-alltheicon icon) " " title))

  (defun hydra-title-with-mat-icon (icon title)
    (concat (all-the-icons-material icon) " " title)))

(use-package pretty-hydra
  :demand t
  :preface
  (defun hydras-add-quit-bindings (result)
    (append '(("q" nil :exit t) ("<escape>" nil :exit t)) result))
  :config
  (advice-add
   #'pretty-hydra--get-heads
   :filter-return #'hydras-add-quit-bindings))

(pretty-hydra-define buffers
  (:hint nil :color teal :title (hydra-title-with-mat-icon "description" "Buffers"))
  ("Switch"
   (("b" #'consult-buffer "switch to" :exit t)
    ("l" #'evil-switch-to-windows-last-buffer "switch to last" :exit t)
    ("o" #'other-window "other buffer" :exit t)
    ("n" #'next-buffer "next" :exit nil)
    ("p" #'previous-buffer "previous" :exit nil))
   "Manage"
   (("h" #'bury-buffer "bury")
    ("k" #'kill-current-buffer "kill")
    ("w" #'save-buffer "save"))))

(pretty-hydra-define comments
  (:hint nil :color teal :title (hydra-title-with-mat-icon "comment" "Comments"))
  ("Toggle Comments"
   (("s" #'comment-or-uncomment-sexp "sexp")
    ("l" #'evilnc-comment-or-uncomment-lines "lines")
    ("p" #'evilnc-comment-or-uncomment-paragraphs "paragraphs")
    ("r" #'comment-or-uncomment-region "region"))))

(pretty-hydra-define consult
  (:hint nil :color teal :title (hydra-title-with-mat-icon "menu" "Consult"))
  ("Consult"
   (("a" #'consult-apropos "apropos")
    ("b" #'consult-buffer "buffer")
    ("o" #'consult-buffer-other-window "buffer (other window)")
    ("f" #'consult-find "find")
    ("g" #'consult-grep "grep")
    ("i" #'consult-imenu "imenu")
    ("p" #'consult-project-imenu "imenu project")
    ("k" #'consult-kmacro "kmacro")
    ("l" #'consult-line "line")
    ("s" #'consult-isearch "isearch")
    ("x" #'consult-flycheck "flycheck"))
   "Mark"
   (("mm" #'consult-mark "mark")
    ("mg" #'consult-global-mark "global mark"))
   "Register"
   (("rr" #'consult-register "register")
    ("rf" #'consult-register-format "register format")
    ("rl" #'consult-register-load "register load")
    ("rs" #'consult-register-store "register store")
    ("rw" #'consult-register-window "register wondow"))
   "Yank"
   (("yy" #'consult-yank "yank")
    ("yp" #'consult-yank-pop "yank pop")
    ("yr" #'consult-yank-replace "yank replace"))))

(pretty-hydra-define errors
  (:hint nil :color teal :title (hydra-title-with-mat-icon "error_outline" "Errors"))
  ("Navigation"
   (("n" flycheck-next-error "next" :color vmacs-red)
    ("p" flycheck-previous-error "previous" :color vmacs-red))
   "Actions"
   (("r" flycheck-buffer "run checks")
    ("c" flycheck-clear "clear")
    ("e" flycheck-list-errors "list"))
   "Checkers"
   (("h" flycheck-describe-checker "describe...")
    ("s" flycheck-select-checker "select...")
    ("v" flycheck-verify-setup "verify"))))

(pretty-hydra-define git
  (:hint nil :color teal :title (hydra-title-with-aicon "git" "Git"))
  ("Git"
   (("s" magit-status "status")
    ("l" magit-log "log...")
    ("L" magit-log-buffer-file "log buffer")
    ("b" magit-blame "blame...")
    ("w" magit-worktree-status "worktree status")
    ("W" magit-worktree "worktree..."))))

(pretty-hydra-define help
  (:hint nil :color teal :title (hydra-title-with-mat-icon "help" "Help"))
  (""
   (("i" #'info "info")
    ("c" #'list-colors-display "colours")
    ("m" #'man "man"))
   "Describe"
   (("db" #'describe-binding "describe bindings")
    ("df" #'describe-function "describe function")
    ("dk" #'describe-key "describe key")
    ("dm" #'describe-mode "describe mode")
    ("dt" #'describe-face "describe face")
    ("dv" #'describe-variable "describe variable"))
   "Apropos"
   (("aa" #'apropos "apropos")
    ("ad" #'apropos-documentation "apropos documentation")
    ("ac" #'apropos-command "apropos command")
    ("al" #'apropos-library "apropos library")
    ("au" #'apropos-user-option "apropos user option")
    ("av" #'apropos-variable "apropos variable")
    ("aV" #'apropos-value "apropos value"))))

(pretty-hydra-define jump
  (:hint nil :color teal :title (hydra-title-with-mat-icon "call_split" "Jump"))
  ("Xref"
   (("j" #'dumb-jump-go "jump")
    ("b" #'xref-pop-marker-stack "back")
    ("p" #'xref-find-definitions "prompt")
    ("P" #'xref-find-definitions-other-window "prompt (other window)")
    ("r" #'xref-find-references "references")
    ("a" #'xref-find-apropos "apropos")
    ("l" #'dumb-jump-quick-look "quick look"))))

(pretty-hydra-define narrow
  (:hint nil :color teal :title (hydra-title-with-mat-icon "photo_size_select_small" "Narrow"))
  ("Narrow to..."
   (("f" #'narrow-to-defun "function")
    ("r" #'narrow-to-region "region")
    ("s" #'org-narrow-to-subtree "org subtree")
    ("S" #'org-tree-to-indirect-buffer))
   "Actions"
   (("w" #'widen "widen"))))

(pretty-hydra-define org
  (:hint nil :color teal :title (hydra-title-with-mode-icon 'org-mode "Org"))
  ("Agenda"
   (("a" #'org-agenda "agenda"))
   "Browse"
   (("o" #'org-jump-to-inbox "jump to inbox")
    ("O" #'org-jump-to-gtd "jump to gtd"))
   "Capture"
   (("c" #'org-capture "capture")
    ("k" #'org-quick-capture "quick capture")
    ("i" #'org-insert-link "insert link")
    ("s" #'org-store-link "store link"))
   "Roam"
   (("<tab>" #'org-roam-buffer-toggle-display "toggle backlinks")
    ("rc" #'org-roam-capture "org-roam capture")
    ("rf" #'org-roam-find-file "find org-roam file")
    ("rh" #'org-roam-find-index "jump to org-roam index")
    ("ri" #'org-roam-insert "insert file link")
    ("rs" #'org-store-link "store link"))
   "Journal"
   (("j" #'org-journal-new-entry "new entry"))))

(pretty-hydra-define search
  (:hint nil :color teal :title (hydra-title-with-mat-icon "search" "Search"))
  ("Search"
   (("g" #'consult-ripgrep "Grep"))
   "Replace"
   (("r" #'vr/replace "Regex replace")
    ("q" #'vr/query-replace "Regex query replace"))))

(pretty-hydra-define main-dispatcher
  (:hint nil :color teal :title (hydra-title-with-fileicon "emacs" "vmacs"))
  ("Menus"
   (("b" buffers/body "buffers...")
    ("c" comments/body "comments...")
    ("e" errors/body "errors...")
    ("g" git/body "git...")
    ("h" help/body "help...")
    ("m" consult/body "consult...")
    ("n" narrow/body "narrow...")
    ("o" org/body "org...")
    ("s" search/body "search...")
    ("x" jump/body "jump..."))
   "Jump To"
   (("f" #'git-modified-files "modified files")
    ("j" #'git-files "tracked files")
    ("t" #'find-project-todo-file "project TODO")
    ("I" #'jump-to-init "init")
    ("L" #'jump-to-layer "layer")
    ("O" #'org-jump-to-inbox "org-inbox")
    ("R" #'org-roam-find-index "org-roam index"))
   "Actions"
   (("!" #'shell-command "run shell command")
    (":" #'eval-expression "evaluate lisp")
    ("." #'eval-last-sexp "evaluate last sexp")
    ("b" #'beginning-of-defun "beginning of defun")
    ("d" #'dired "dired")
    ("D" #'dired-other-window "dired (other window)")
    ("u" #'universal-argument "universal argument")
    ("v" #'vterm "vterm")
    ("w" #'save-buffer "save")
    ("z" #'selectrum-repeat "selectrum repeat"))))

(use-package general
  :config
  (progn
    (general-setq
     general-override-states
     '(insert emacs hybrid normal visual motion operator replace))
    (general-override-mode +1)
    (general-define-key
     :states '(normal visual motion)
     :keymaps 'override "," 'main-dispatcher/body)))

(provide 'vmacs-hydras)
;;; vmacs-hydras.el ends here