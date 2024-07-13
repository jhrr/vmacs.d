;;; vmacs-org.el --- Configure org-mode. -*- lexical-binding: t; -*-

;;; Commentary:

;; "Capture a little wave."

;;; Code:

(use-package org-transclusion
  :straight t
  :commands (org-transclusion-add-all))

(use-package org
  :straight t
  :mode
  ("\\.org\\'" . org-mode)
  :mode-hydra
  ;; TODO: Can't we use the same one defined in vmacs-hydras?
  (org-mode
   ("Agenda"
    (("a" #'org-agenda "agenda"))
    "Browse"
    (("o" #'org-jump-to-inbox "jump to inbox")
     ("g" #'org-jump-to-gtd "jump to gtd file")
     ("i" #'org-roam-jump-to-index "jump to org-roam index")
     ("k" #'org-roam-node-find "jump to org-roam file")
     ("R" #'org-transclusion-refresh "refresh transclusion node")
     ("t" #'org-transclusion-move-to-source "jump to transclusion source"))
    "Capture"
    (("c" #'org-capture "capture")
     ("q" #'org-quick-capture "quick capture"))
    "Links"
    (("ls" #'org-store-link "store link")
     ("li" #'org-insert-link "insert link"))
    "Roam"
    (("rb" #'org-roam-buffer-toggle "toggle org-roam buffer")
     ("ri" #'org-roam-node-insert "insert a link to an org-roam node")
     ("rc" #'org-roam-capture "org-roam capture"))
    "Journal"
    (("j" #'org-journal-new-entry "new entry"))))
  :preface
  (defun directory-org (dir)
    "Return all org files under a DIR."
    (directory-files dir t "\.org$" nil))

  (defun org-quick-capture ()
    "Capture an item without going through template selection."
    (interactive)
    (org-capture nil "q"))

  (defun org-jump-to-inbox ()
    "Open the `org-default-notes-file' file in another window."
    (interactive)
    (find-file org-default-notes-file))

  (defun org-jump-to-gtd ()
    "Jump to a selected file in the `org-gtd-directory'."
    (interactive)
    (jump-to-file (filename-map (directory-org org-gtd-directory))))

  ;; TODO: Decouple template from (insert), just pass in a reference.
  (defun org-file-property-template ()
    (interactive)
    (insert ":PROPERTIES:"
            (concat "\n:ID: " (org-id-uuid))
            "\n:CATEGORY:"
            "\n:END:"
            "\n#+TITLE: "))

  (defun org-file-matter-property-template ()
    (interactive)
    (insert ":PROPERTIES:"
            (concat "\n:ID: " (org-id-uuid))
            "\n:CATEGORY: matter"
            "\n:ROAM_ALIASES:"
            "\n:END:"
            "\n#+TITLE: "))

  (defun org-file-reading-property-template ()
    (interactive)
    (insert ":PROPERTIES:"
            (concat "\n:ID: " (org-id-uuid))
            "\n:CATEGORY: reading"
            "\n:Title:"
            "\n:Authors:"
            "\n:Date:"
            "\n:END:"
            "\n#+TITLE: "))

  (defun org-file-work-property-template ()
    (interactive)
    (insert ":PROPERTIES:"
            (concat "\n:ID: " (org-id-uuid))
            "\n:CATEGORY: work.???"
            "\n:Project:"
            "\n:Key:" ;; Directories on disk should share this name/key.
            "\n:ROAM_ALIASES:"
            "\n:END:"
            "\n#+TITLE: "))

  (defun grep-org ()
    "Search through the org directory tree."
    (interactive)
    (consult-ripgrep user-org-directory))

  (defun grep-org-roam ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (consult-ripgrep org-roam-directory))

  :init
  (defvar user-org-directory
    (expand-file-name "org/" user-dropbox-directory))
  (defvar org-archive-directory
    (expand-file-name "archive/" user-org-directory))
  (setq org-default-notes-file
        (expand-file-name "gtd-inbox.org" user-org-directory))

  (setq org-capture-bookmark nil)
  (setq org-capture-templates
        '(("q" "Quick" entry (file+headline org-default-notes-file "CAPTURE")
           "* %? ")))

  (setq org-adapt-indentation nil)
  (setq org-agenda-span 'day)
  (setq org-agenda-sticky t)
  (setq org-ellipsis " ...")
  (setq org-fold-core-style 'overlays)
  (setq org-log-into-drawer t)
  (setq org-property-format "%s %s") ;; Don't tab/align property values.
  (setq org-startup-folded t)
  (setq org-treat-insert-todo-heading-as-state-change nil)

  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "|" "DONE(d!)")
          (sequence "DELEGATED" "|" "BLOCKED(b@/!)" "CANCELLED(c@/!)")))

  (setq org-todo-keyword-faces
        '(("TODO" :foreground "#009cff" :weight bold)
          ("NEXT" :foreground "#ffc900" :weight bold)
          ("DONE" :foreground "#89a976" :weight bold)
          ("DELEGATED" :foreground "#75a2a9" :weight bold)
          ("BLOCKED" :foreground "#f76050" :weight bold)
          ("CANCELLED" :foreground "#f76050" :weight bold)))

  (add-hook 'org-mode-hook
            #'(lambda () (progn
                      (lambda () (push '("--" . ?—) prettify-symbols-alist))
                      (setq fill-column 70)
                      (setq evil-shift-width 2)
                      (turn-on-auto-fill)
                      (org-transclusion-add-all))))
  :config
  (use-package org-contrib :straight t)
  (use-package org-habit
    :config
    (add-to-list 'org-modules 'org-habit t)
    (setq org-habit-following-days 7
          org-habit-preceding-days 35
          org-habit-show-all-today t))
  (require 'org-checklist)

  (org-defkey org-mode-map (kbd "RET") #'(lambda () (interactive) (org-return nil))))

(use-package org-roam
  :straight t
  :commands
  (org-roam-capture
   org-roam-db-query
   org-roam-node-insert
   org-roam-node-visit)
  :custom
  (org-roam-database-connector 'sqlite-builtin)
  :preface
  (defmacro def-org-roam-subdirectory (name)
    "Register subdirectories with org-roam."
    (let ((binding (intern (concat "org-roam-" name "-directory"))))
      `(defvar ,binding (expand-file-name (concat ,name "/") org-roam-directory))))

  (defun org-roam-get-files-with-property (property)
    (-flatten (org-roam-db-query
               [:select [id]
                        :from nodes
                        :where (like properties $r1)]
               (format "%%%s%%" property))))

  (defun org-roam-jump-to-index ()
    (interactive)
    (org-roam-node-visit
     (org-roam-node-from-id
      (caar (or (org-roam-db-query
                 [:select id :from nodes :where (= title "Index") :limit 1])
                (user-error "No node with title Index"))))))

  :init
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))

  (setq org-roam-directory (expand-file-name "index/" user-org-directory))
  (defvar org-roam-index-file (expand-file-name "index.org" org-roam-directory))

  (seq-do
   (lambda (subdir) (eval `(def-org-roam-subdirectory ,subdir)))
   '("life"
     "matter"
     "reading"
     "work"
     "writing"))

  :config
  (org-roam-db-autosync-mode))

(use-package org-journal
  :straight t
  :defer t
  :mode-hydra
  (org-journal-mode
   ("Entry"
    (("j" #'org-journal-new-entry "new entry"))
    "Browse"
    (("t" #'journal-file-today "today")
     ("y" #'journal-file-yesterday "yesterday")
     ("l" #'journal-last-year  "last year")
     ("p" #'org-journal-previous-entry "previous")
     ("d" #'get-specific-journal-entry "by date"))
    "Save"
    (("s" #'org-journal-save-entry "save")
     ("x" #'org-journal-save-entry-and-exit "save and exit"))))
  :preface
  (setq org-journal-dir
        (expand-file-name "journal/" user-org-directory))

  (defun iso-week ()
    "Return the ISO week number, human readable and Monday indexed."
    (concat "Week " (format-time-string "%V")))

  (defun get-offset-date (offset)
    "Calculate a date OFFSET from the current time."
    (time-subtract (current-time) (days-to-time offset)))

  (defun get-journal-file-by-offset (offset)
    "Return the filename for a journal entry modulated by a date OFFSET."
    (format-time-string "%Y%m%d" (get-offset-date offset)))

  (defun find-journal-file (offset)
    "Find and load a journal file, if it exists, by a date OFFSET from today."
    (let* ((file-name (get-journal-file-by-offset offset))
           (journal-file (expand-file-name (concat org-journal-dir file-name))))
      (if (file-exists-p journal-file) (find-file-other-window journal-file)
        (message
         (format "The journal file for date %s does not exist." file-name)))))

  (defun journal-file-today ()
    "Load the journal file for today's date, if it exists."
    (interactive)
    (find-journal-file 0))

  (defun journal-file-yesterday ()
    "Load the journal file for yesterday's date, if it exists."
    (interactive)
    (find-journal-file 1))

  (defun journal-last-year ()
    "Load the journal file for this day one year ago."
    (interactive)
    (find-journal-file 365))

  (defun journal-all-entries ()
    "List all journal files."
    (interactive)
    (sort
     (directory-files org-journal-dir nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")
     #'string-greaterp))

  (defun get-specific-journal-entry ()
    "Prompt to open the journal file for a specific date."
    (interactive)
    (let* ((target-journal-file
            (concat org-journal-dir
                    (completing-read
                     "date: " (journal-all-entries)))))
      (find-file-other-window target-journal-file)))

  ;; TODO: Merge into single fun.
  (defun org-journal-save-entry ()
    (interactive)
    (save-buffer))

  (defun org-journal-save-entry-and-exit ()
    (interactive)
    (save-buffer)
    (kill-buffer-and-window))

  (setq org-journal-date-prefix "#+TITLE: ")
  (setq org-journal-date-format (concat "%Y/%m/%d, " (iso-week) ", %A"))
  (setq org-journal-time-prefix "* ")
  (setq org-journal-time-format "%R "))

(provide 'vmacs-org)
;;; vmacs-org.el ends here
