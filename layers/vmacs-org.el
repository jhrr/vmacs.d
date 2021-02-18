;;; vmacs-org.el -- Configure org-mode. -*- lexical-binding:t -*-

;;; Commentary:

;; "Capture a little wave."

;;; Code:

;; TODO: (def create-new-gtd-file)
;; Adds the ~#+ARCHIVE: %s_done::~ to top.
;; defhydra hydra--org
;;   - agenda
;;   - capture
;;   - clocking
;;   - go to clocked item
;;   - journal new entry, open today's journal
;; defhydra hydra--org-agenda -> subhydra necessary?
;; defhydra hydra--org-clock -> subhydra necessary?
;; State -> TODO NEXT BLOCKED DONE
;; Category -> TASK PROJECT META

(use-package org-plus-contrib
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :init
  (defvar user-org-directory
    (expand-file-name "org/" user-dropbox-directory))
  (defvar org-archive-directory
    (expand-file-name "archive/" user-org-directory))
  (defvar org-gtd-directory
    (expand-file-name "dasein/" user-org-directory))
  (defvar org-work-directory
    (expand-file-name "work/" user-org-directory))
  (defvar org-default-notes-file
    (expand-file-name "gtd-inbox.org" user-org-directory))

  (setq org-capture-templates
        '(("q" "Quick" entry
           (file+headline org-default-notes-file "Quick")
           "* %?\n  %t" :clock-in t :clock-resume t)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "BLOCKED(b@/!)" "|" "CANCELLED(c@/!)" "MEETING")))

  ;; TODO: Merge colors with vmacs-theme.el
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("NEXT" :foreground "blue" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("BLOCKED" :foreground "orange" :weight bold)
          ("CANCELLED" :foreground "black" :weight bold)
          ("MEETING" :foreground "blue" :weight bold)))

  (setq org-adapt-indentation nil)
  (setq org-agenda-span 'day)
  (setq org-agenda-sticky t)
  (setq org-ellipsis " …")
  (setq org-startup-folded t)
  ;; Activate org-columns with C-c C-x C-c while on a top-level heading.
  ;; (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  ;; (setq org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))

  (defun gtd ()
    "List all files in the `org-gtd-directory'."
    (seq-filter
     (lambda (path) (not (string-prefix-p ".#" path)))
     (directory-files org-gtd-directory t "\.org$" nil)))

  (defun jump-to-gtd ()
    "Jump to a selected file in the `org-gtd-directory'."
    (interactive)
    (jump-to-file (filename-map (gtd))))
  (bind-key* "C-c o" 'jump-to-gtd)

  (defun jump-to-inbox ()
    "Open the `org-default-notes-file' file in another window."
    (interactive)
    (find-file org-default-notes-file))
  (bind-key* "C-c O" 'jump-to-inbox)

  (defun quick-capture ()
    "Capture an item without going through template selection."
    (interactive)
    (org-capture nil "q"))
  (bind-key* "C-. ;" 'quick-capture)

  (add-hook 'org-mode-hook '(lambda () (linum-mode -1)))
  :config
  (require 'org-checklist))

(use-package org-journal
  :straight t
  :bind
  ("C-. C-j" . org-journal-new-entry)
  :init
  (setq org-journal-dir
        (expand-file-name "journal/" org-archive-directory))
  :config
  (setq org-journal-date-format (concat "%Y/%m/%d, " (iso-week) ", %A"))
  (setq org-journal-date-prefix "#+TITLE: ")

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
    "Use ido to load the journal file for a specific date."
    (interactive)
    (let* ((selectrum-should-sort-p nil)
           (target-journal-file
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

  (bind-key "C-. j l" #'journal-last-year 'org-journal-mode-map)
  (bind-key "C-. j t" #'journal-file-today 'org-journal-mode-map)
  (bind-key "C-. j y" #'journal-file-yesterday 'org-journal-mode-map)
  (bind-key "C-. j a" #'get-specific-journal-entry 'org-journal-mode-map)
  (bind-key "C-. j p" #'org-journal-previous-entry 'org-journal-mode-map)
  (bind-key "C-. j s" #'org-journal-save-entry 'org-journal-mode-map)
  (bind-key "C-. j x" #'org-journal-save-entry-and-exit 'org-journal-mode-map))

;; (use-package org-roam
;;   :straight t
;;   :bind
;;   (("C-. n" . org-roam-capture)
;;    ("C-. C-n" . org-roam-find-file))
;;   :init
;;   (setq org-roam-directory (expand-file-name "index/" user-org-directory))))

(provide 'vmacs-org)
;;; vmacs-org.el ends here
