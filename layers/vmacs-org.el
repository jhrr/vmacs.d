;;; vmacs-org.el -*- lexical-binding:t -*-

;;; Commentary: Configure org-mode.

; Simple is better with org-mode, it gets out of control and confusing fast.
; This configuration is not really about using org-mode for planning, it's
; about capturing the CHAOS.

; 1. Capture templates (inbox and vocab only).
; 2. Refiling from inbox.
; 3. Journal.

;;; Code:

; https://github.com/raxod502/straight.el#installing-org-with-straightel
; (require 'subr-x)
; (straight-use-package 'git)

; (defun org-git-version ()
;   "The Git version of org-mode.
; Inserted by installing org-mode or when a release is made."
;   (require 'git)
;   (let ((git-repo (expand-file-name
;                    "straight/repos/org/" user-emacs-directory)))
;     (string-trim
;      (git-run "describe"
;               "--match=release\*"
;               "--abbrev=6"
;               "HEAD"))))

; (defun org-release ()
;   "The release version of org-mode.
; Inserted by installing org-mode or when a release is made."
;   (require 'git)
;   (let ((git-repo (expand-file-name
;                    "straight/repos/org/" user-emacs-directory)))
;     (string-trim
;      (string-remove-prefix
;       "release_"
;       (git-run "describe"
;                "--match=release\*"
;                "--abbrev=0"
;                "HEAD")))))

; (provide 'org-version)
; (straight-use-package 'org)

(defvar user-org-directory (concat user-dropbox-directory "org/"))
(defvar org-archive-directory (concat user-org-directory "archive/"))
(setq org-directory user-org-directory)
(setq org-default-notes-file (concat user-org-directory "gtd-inbox.org"))
(setq org-journal-dir (concat org-archive-directory "journal/"))

(defun vmacs-org/jump-to-inbox ()
  "Open org-inbox in another window."
  (interactive)
  (find-file org-default-notes-file))

(bind-key* "C-. o" 'vmacs-org/jump-to-inbox)

(setq org-capture-templates
      '(("q" "Quick" entry
         (file+headline org-default-notes-file "Quick") "* %?\n  %t")))

(defun vmacs-org/quick-capture ()
  "Capture an item using the quick template.
Don't go through the template selection screen."
  (interactive)
  (org-capture nil "q"))

(bind-key* "C-. c" 'vmacs-org/quick-capture)

; TODO: Prefix all funs with vmacs/
; (defun iso-week ()
;   "Return the ISO week number, human readable and Monday indexed."
;   (concat "Week " (format-time-string "%V")))

; (use-package org-journal
;   :straight t
;   :bind ("C-c C-j" . org-journal-new-entry)
;   :config
;     (progn
;       (setq org-journal-date-format (concat "%Y/%m/%d, " (iso-week) ", %A"))
;       (setq org-journal-date-prefix "#+TITLE: ")

;       (defun get-offset-date (offset)
;         "Calculate a date OFFSET from the current time."
;         (time-subtract (current-time) (days-to-time offset)))

;       (defun get-journal-file-by-offset (offset)
;         "Return the filename for a journal entry modulated by a date OFFSET."
;         (format-time-string "%Y%m%d" (get-offset-date offset)))

;       (defun find-journal-file (offset)
;         "Find and load a journal file, if it exists, by a date OFFSET from today."
;         (let* ((file-name (get-journal-file-by-offset offset))
;                 (journal-file (expand-file-name (concat org-journal-dir file-name))))
;           (if (file-exists-p journal-file) (find-file-other-window journal-file)
;             (message
;               (format "The journal file for date %s does not exist." file-name)))))

;       (defun journal-file-today ()
;         "Load the journal file for today's date, if it exists."
;         (interactive)
;         (find-journal-file 0))

;       (defun journal-file-yesterday ()
;         "Load the journal file for yesterday's date, if it exists."
;         (interactive)
;         (find-journal-file 1))

;       (defun journal-last-year ()
;         "Load the journal file for this day one year ago."
;         (interactive)
;         (find-journal-file 365))

;       (defun get-specific-journal-entry ()
;         "Use ido to load the journal file for a specific date."
;         (interactive)
;         (let* ((all-journal-entries
;                  (directory-files org-journal-dir
;                                   nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
;                  (target-journal-file
;                    (concat org-journal-dir
;                            (ido-completing-read "date: " all-journal-entries))))
;             (find-file-other-window target-journal-file)))

;       (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))

;       (bind-key* "C-C C-j" 'org-journal-new-entry)
;       (bind-key* "C-c j a" 'get-specific-journal-entry)
;       (bind-key* "C-c j t" 'journal-file-today)
;       (bind-key* "C-c j y" 'journal-file-yesterday)
;       (bind-key* "C-c j l" 'journal-last-year)))

(provide 'vmacs-org)
;;; vmacs-org.el ends here
