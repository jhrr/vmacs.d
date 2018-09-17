;;; vmacs-search.el -*- lexical-binding:t -*-

;;; Commentary: Configure grepping and finding.

;;; Code:

; # TODO: Rebind this entire flow to be super easy.
; # TODO: Hook into system $GREPPAGE.
; # TODO: Config wgrep and multiple cursors on grep call.
; # TODO: Use seq instead of dash: --each.

; (use-package grep
;   :init
;   (progn
;     (defun rgrep-fullscreen (regexp &optional files dir confirm)
;       "Open grep in full screen, saving windows."
;       (interactive
;        (progn
;          (grep-compute-defaults)
;          (cond
;           ((and grep-find-command (equal current-prefix-arg '(16)))
;            (list (read-from-minibuffer "Run: " grep-find-command
;                                        nil nil 'grep-find-history)))
;           ((not grep-find-template)
;            (error "grep.el: No `grep-find-template' available"))
;           (t (let* ((regexp (grep-read-regexp))
;                     (files (grep-read-files regexp))
;                     (dir (ido-read-directory-name "Base directory: "
;                                                   nil default-directory t))
;                     (confirm (equal current-prefix-arg '(4))))
;                (list regexp files dir confirm))))))
;       (window-configuration-to-register ?$)
;       (rgrep regexp files dir confirm)
;       (switch-to-buffer "*grep*")
;       (delete-other-windows)
;       (beginning-of-buffer))

;     (defun rgrep-quit-window ()
;       (interactive)
;       (kill-buffer)
;       (jump-to-register ?$))

;     (defun rgrep-goto-file-and-close-rgrep ()
;       (interactive)
;       (compile-goto-error)
;       (kill-buffer "*grep*")
;       (delete-other-windows)
;       (message "Type C-x r j $ to return to pre-rgrep windows."))

;     (defvar grep-match-positions nil)
;     (make-variable-buffer-local 'grep-match-positions)

;     (defun grep-register-match-positions ()
;       ;; Emacs 25.1 - save-excursion -> save-mark-and-excursion
;       (save-excursion
;         (forward-line 0)
;         (let ((end (point)) beg)
;           (goto-char compilation-filter-start)
;           (forward-line 0)
;           (setq beg (point))
;           ;; Only operate on wholenump lines so we don't get caught
;           ;; with part of an escape sequence in one chunk and the rest
;           ;; in another.
;           (when (< (point) end)
;             (setq end (copy-marker end))
;             ;; Register all positions of matches
;             (while (re-search-forward "\033\\[0?1;31m\\(.*?\\)\033\\[[0-9]*m" end 1)
;               (add-to-list 'grep-match-positions (set-marker (make-marker) (match-beginning 1))))))))

;     (defadvice grep-mode (after grep-register-match-positions activate)
;       (add-hook 'compilation-filter-hook 'grep-register-match-positions nil t))

;     (bind-key "C-. g" 'rgrep-fullscreen)
;     (bind-key "C-. C-a" 'mc/add-cursors-to-all-matches grep-mode-map)
;     (bind-key "C-. C-w" 'wgrep-change-to-wgrep-mode grep-mode-map)
;     (bind-key "C-x M-S" 'wgrep-save-all-buffers grep-mode-map)
;     (bind-key "C-M-<return>" 'rgrep-goto-file-and-close-rgrep grep-mode-map)
;     (bind-key "q" 'rgrep-quit-window grep-mode-map)))

; (use-package multiple-cursors
;   :defer t
;   :commands (mc/maybe-multiple-cursors-mode
;              mc/create-fake-cursor-at-point)
;   :init
;   (progn
;     (setq mc/list-file "~/.emacs.d/.caches/.mc-lists.el")

;     (defun mc/add-cursors-to-all-matches ()
;       (interactive)
;       (--each grep-match-positions
;         (unless (= 0 it-index)
;           (mc/create-fake-cursor-at-point))
;         (goto-char it))
;       (mc/maybe-multiple-cursors-mode)))

;   :config
;   (progn
;     (add-to-list 'mc--default-cmds-to-run-once
;                  'mc/add-cursors-to-all-matches)))

; (use-package wgrep
;   :defer t
;   :commands (wgrep-change-to-wgrep-mode))

(provide 'vmacs-search)
;;; vmacs-search.el ends here
